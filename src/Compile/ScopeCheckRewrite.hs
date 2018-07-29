module Compile.ScopeCheck (checkProgramScope, checkFunction) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Tuple
import Data.Maybe

import qualified Compile.Types.EAST as E
import Compile.Types
import Debug.Trace

type VarDeclSet = Set.Set Ident
type VarDefSet = Set.Set Ident
type FuncSet = Set.Set Ident

type ScopeState = (VarDeclSet, VarDefSet, FuncSet)
checkFunction :: (ScopeState, Bool) -> E.Gdecl -> (ScopeState, Bool)
checkFunction (scopestate, previousValid) (Fdefn ctype ident paramlist block) =
  let
    stateWithFunction = declareFunction scopestate ident
    stateWithParams = declareParams paramlist
    -- Don't want shit declared in our block to leak over to the next gdecl
    (garbageState, valid) = checkBlock stateWithFunction block
  in
    -- Return the state with our new function declared
    -- Do NOT return state with params and shit declared in our function!!
    (stateWithFunction, valid && previousValid)

-- Glorified foldl wrapper
checkBlock :: ScopeState -> E.Block -> (ScopeState, Bool)
checkBlock scopestate (stmt:block) =
  let
    (newState, valid) = checkStmt stmt
    (finalState, finalValid) = checkBlock newState block
  in
    (finalState, valid && finalValid)

checkBlock scopestate [] = (scopestate, True)

-- Glorified wrapper again
checkStmt :: ScopeState -> E.Stmt -> (ScopeState, Bool)
checkStmt scopestate (Simp simp) = checkSimp fullState simp
checkStmt scopestate (Ctrl ctrl) = 
  let
    -- Enforce that defined variables go through
    (newState, valid) = checkCtrl fullState ctrl
  in
    (updateBlockVariables scopestate newState, valid)
checkStmt scopestate (Blk block) = 
  let
    -- Enforce that defined variables go through
    (newState, valid) = checkBlock fullState block
  in
    (updateBlockVariables scopestate newState, valid)

updateBlockVariables (setDeclare1, setDefine1, )

checkSimp :: ScopeState -> E.Simp -> (ScopeState, Bool)
checkSimp scopestate (Asgn lvalue asnop expr) =
  case (checkExp scopestate expr, asnop) of
    -- YEAH! Our lvalue is defined by this Asgn!!
    ((newState, True), Equal) -> defineLvalue scopestate lvalue
    -- Aww... Our lvalue is only modified here. Better check if it's already defined
    ((newState, True), _) -> checkLvalue scopestate lvalue
    -- WTF?
    ((_, False), _) -> error ("checkExp failed!")

checkSimp scopestate (Decl ctype ident) = (declareVar scopestate ident, True)

checkSimp scopestate (Exp expr) = checkExp expr  

checkCtrl :: ScopeState -> E.Ctrl -> ScopeState
checkCtrl scopestate (If expr stmt1 stmt2) =
  let
    (_, exprValid) = checkExp scopestate expr
    (state2, stmt1Valid) = checkStmt scopestate stmt1
    (state3, stmt2Valid) = checkStmt scopestate stmt2
  in
    case (exprValid, stmt1Valid, stmt2Valid) of
      -- We MUST perform set intersection on declared variable set and defined variable set
      (True, True, True) -> (updateIfElseState scopestate state2 state3, True)
      _ -> error("checkCtrl failed")
checkCtrl scopestate (While expr stmt) =
  let
    (_, exprValid) = checkExp scopestate expr
    -- While loops may not be executed!!
    (_, stmtValid) = checkStmt scopestate stmt
  in
    case (exprValid, stmtValid) of
      (True, True) -> (scopestate, True)
      (False, _) -> error("While: Invalid expression" ++ show expr)
      (_, False) -> error("While: Invalid stmt" ++ show stmt)

checkCtrl scopestate (Ret expr) =
  case checkExp scopestate expr of
    (_, True) -> (defineAllDeclaredVariables scopestate, True)
    (_, False) -> error("Return: invalid expr:" ++ show expr)

checkCtrl scopestate RetVoid = (defineAllDeclaredVariables scopestate, True)

checkExp :: ScopeState -> Exp -> Bool
checkExp scopestate (Const n) = True
checkExp scopestate (CTrue ) = True
checkExp scopestate (CFalse ) = True
checkExp scopestate (Var ident) = isDef scopestate ident
checkExp scopestate (Unary unop expr) = checkExp scopestate expr
checkExp scopestate (Binary binop expr1 expr2) = checkExp scopestate expr1 && checkExp scopestate expr2
checkExp scopestate (Ternary expr1 expr2 expr3) = checkExp scopestate expr1 && checkExp scopestate expr2 && checkExp scopestate expr3
checkExp scopestate (Call ident args) =
  let
    argValid = (all (checkExp scopestate) args)
    funcValid = isFunctionDefined scopestate ident
  in
    case (argValid, funcValid) of
      (False, _) -> error ("checkExp Call: arg not valid!" ++ show ident ++ " args:" ++ show args)
      (_, False) -> error ("checkExp Call: function name not found!" ++ show ident ++ " args:" ++ show args)
      (True, True) -> True

checkExp scopestate (Dot expr ident) = checkExp scopestate expr
checkExp scopestate (Alloc ctype) = True
checkExp scopestate (PointerStar expr) = checkExp scopestate expr
checkExp scopestate (AllocArray ctype expr) = checkExp scopestate expr
checkExp scopestate (ArrayAccess expr1 expr2) = checkExp scopestate expr1 && checkExp scopestate expr2
checkExp scopestate (NULL) = True



isDec :: Set.Set Ident -> Set.Set Ident -> Ident -> Bool
isDec setDeclare1 setDefine1 ident = 
  case (Set.member ident setDeclare1) of
    True -> True
    False -> error ("Variable not declared:" ++ show ident)

isDef :: Set.Set Ident -> Set.Set Ident -> Ident -> Bool
isDef setDeclare1 setDefine1 ident = 
  case (Set.member ident setDeclare1) && (Set.member ident setDefine1) of
    True -> True
    False -> error("Variable not defined:" ++ show ident)


defineAllDeclaredVariables (setDeclare1, setDefine1, setFunction1) (setDeclare2, setDefine2, setFunction2) =
  (setDeclare1, Set.intersection setDeclare1 setDefine2, setFunction2)

updateIfElseState (setDeclare1, setDefine1, setFunction1) (setDeclare2, setDefine2, setFunction2) (setDeclare3, setDefine3, setFunction3) =
  -- Declared variables should be those declared before the If statement
  -- Defined variables should be the variables defined both in the If statement and the Else statement
  (setDeclare1, Set.intersection setDeclare1 (Set.intersection setDefine2 setDefine3), setFunction1)

isFunctionDefined (setDeclare, setDefine, setFunction) functionName =
  Set.member functionName setFunction

