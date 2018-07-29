module Compile.ScopeCheck (checkFunction) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Tuple
import Data.Maybe

import qualified Compile.Types.EAST as E
import Compile.Types
import Debug.Trace

type IdentSet = Set.Set E.Ident
type FuncSet = IdentSet
-- Check a single function. Should only be used when you know the function you're checking
-- has a Just AST and not a Nothing

checkFunction :: FuncSet -> E.Gdecl -> (FuncSet, Bool)
checkFunction functionSet (E.Fdefn ctype ident paramlist block)=
  let
    newFunctionSet = Set.insert ident functionSet
    startSet = Set.fromList (getIdentsFromParamList paramlist)
    isFunctionDefined functionName = Set.member functionName newFunctionSet
    (_, _, valid) = checkBlock isFunctionDefined startSet startSet block
  in
    (newFunctionSet, valid)

-- Get variable names from params list
getIdentsFromParamList :: [E.Param] -> [E.Ident]
getIdentsFromParamList params = map (\(E.Param ctype ident) -> ident) params

-- Recall: Block = [Stmt]
checkBlock :: (Ident -> Bool) -> Set.Set Ident -> Set.Set Ident -> E.Block -> (Set.Set Ident, Set.Set Ident, Bool)
checkBlock f setDeclare1 setDefine1 (stmt:block) =
  let
    (setDeclare2, setDefine2, valid) = checkStmt f setDeclare1 setDefine1 stmt
  in
    case valid of
      True -> checkBlock f setDeclare2 setDefine2 block
      False -> error ("Invalid statement:" ++ show stmt)
checkBlock _ setDeclare1 setDefine1 [] = (setDeclare1, setDefine1, True)

-- Recall: Simp Simp | Ctrl Ctrl | Blk Block
checkStmt :: (Ident -> Bool) -> Set.Set Ident -> Set.Set Ident -> E.Stmt -> (Set.Set Ident, Set.Set Ident, Bool)
checkStmt f setDeclare1 setDefine1 (E.Simp simp) = checkSimp f setDeclare1 setDefine1 simp
checkStmt f setDeclare1 setDefine1 (E.Ctrl ctrl) = 
  let
    (setDeclare2, setDefine2, ctrlValid) = checkCtrl f setDeclare1 setDefine1 ctrl
  in
    case ctrlValid of
      True -> (setDeclare1, Set.intersection setDeclare1 setDefine2, True)
      False -> error("Invalid ctrl" ++ show ctrl)
checkStmt f setDeclare1 setDefine1 (E.Blk block) = 
  let
    (setDeclare2, setDefine2, blockValid) = checkBlock f setDeclare1 setDefine1 block
  in
    case blockValid of
      True -> (setDeclare1, Set.intersection setDeclare1 setDefine2, True)
      False -> error("Invalid block" ++ show block)

checkSimp :: (Ident -> Bool) -> Set.Set Ident -> Set.Set Ident -> E.Simp -> (Set.Set Ident, Set.Set Ident, Bool)
checkSimp f setDeclare1 setDefine1 (E.Asgn (E.Variable ident) asnop expr) =
  case (checkExp f setDeclare1 setDefine1 expr, isDec setDeclare1 setDefine1 ident) of
    (True, True) -> (setDeclare1, Set.insert ident setDefine1, True)
    (False, _) -> error ("checkSimp: Invalid expression" ++ show expr)
    (_, False) -> error ("checkSimp: ident not declared" ++ show ident)

checkSimp f setDeclare1 setDefine1 (E.Asgn lvalue asnop expr) =
  case (checkExp f setDeclare1 setDefine1 expr, asnop) of
    -- YEAH! Our lvalue is defined by this Asgn!!
    (True, Equal) -> defineLvalue f setDeclare1 setDefine1 lvalue
    -- Aww... Our lvalue is only modified here. Better check if it's already defined
    (True, _) -> (setDeclare1, setDefine1, checkLvalue f setDeclare1 setDefine1 lvalue)
    -- WTF?
    (False, _) -> error ("checkExp failed!")

checkSimp f setDeclare1 setDefine1 (E.Decl ctype ident) = 
  case (Set.member ident setDeclare1, ctype) of
    (True,_) -> error ("Redeclararation of ident:" ++ show ident)
    (False,E.CVoid) -> error ("Cannot declare ident as void")
    (False,_) ->
      (Set.insert ident setDeclare1, setDefine1, True)

checkSimp f setDeclare1 setDefine1 (E.Exp expr) = (setDeclare1, setDefine1, checkExp f setDeclare1 setDefine1 expr)


checkLvalue :: (Ident -> Bool) -> Set.Set Ident -> Set.Set Ident -> E.Lvalue -> Bool
checkLvalue f setDeclare1 setDefine1 (E.Variable ident) = isDef setDeclare1 setDefine1 ident
checkLvalue f setDeclare1 setDefine1 (E.LvalueDot lvalue ident) = checkLvalue f setDeclare1 setDefine1 lvalue
checkLvalue f setDeclare1 setDefine1 (E.LvaluePointerStar lvalue) = checkLvalue f setDeclare1 setDefine1 lvalue
checkLvalue f setDeclare1 setDefine1 (E.LvalueArrayAccess lvalue expr) = (checkLvalue f setDeclare1 setDefine1 lvalue) && (checkExp f setDeclare1 setDefine1 expr)

defineLvalue :: (Ident -> Bool) -> Set.Set Ident -> Set.Set Ident -> E.Lvalue -> (Set.Set Ident, Set.Set Ident, Bool)
defineLvalue f setDeclare1 setDefine1 (E.Variable ident) =
  case isDec setDeclare1 setDefine1 ident of
    -- Insert lvalue into setDefine
    True -> (setDeclare1, Set.insert ident setDefine1, True)
    False -> error("Lvalue not defined")
-- Other lvalues do not define anything
defineLvalue f setDeclare1 setDefine1 lvalue = (setDeclare1, setDefine1, checkLvalue f setDeclare1 setDefine1 lvalue)

checkCtrl :: (Ident -> Bool) -> Set.Set Ident -> Set.Set Ident -> E.Ctrl -> (Set.Set Ident, Set.Set Ident, Bool)
checkCtrl f setDeclare1 setDefine1 (E.If expr stmt1 stmt2) =
  let
    exprValid = checkExp f setDeclare1 setDefine1 expr
    (setDeclare2, setDefine2, stmt1Valid) = checkStmt f setDeclare1 setDefine1 stmt1
    (setDeclare3, setDefine3, stmt2Valid) = checkStmt f setDeclare1 setDefine1 stmt2
  in
    case (exprValid, stmt1Valid, stmt2Valid) of
      (True, True, True) -> (setDeclare1, Set.intersection setDeclare1 (Set.intersection setDefine2 setDefine3), True)
      _ -> error ("checkCtrl: Invalid expression:" ++ show expr)

checkCtrl f setDeclare1 setDefine1 (E.While expr stmt) =
  let
    exprValid = checkExp f setDeclare1 setDefine1 expr
    (setDeclare2, setDefine2, stmtValid) = checkStmt f setDeclare1 setDefine1 stmt
  in
    case (exprValid, stmtValid) of
      (True, True) -> (setDeclare1, setDefine1, True)
      (False, _) -> error("While: Invalid expression" ++ show expr)
      (_, False) -> error("While: Invalid stmt" ++ show stmt)

checkCtrl f setDeclare1 setDefine1 (E.Ret expr) =
  case checkExp f setDeclare1 setDefine1 expr of
    -- Define all variables, so we set setDefine1 to be setDeclare1
    True -> (setDeclare1, setDeclare1, True)
    False -> error("Return: invalid expr:" ++ show expr)
checkCtrl f setDeclare1 setDefine1 E.RetVoid =
  (setDeclare1, setDeclare1, True)

checkExp :: (Ident -> Bool) -> Set.Set Ident -> Set.Set Ident -> E.Exp -> Bool
checkExp f setDeclare1 setDefine1 (E.Const n) = True
checkExp f setDeclare1 setDefine1 (E.CTrue) = True
checkExp f setDeclare1 setDefine1 (E.CFalse ) = True
checkExp f setDeclare1 setDefine1 (E.Var ident) = isDef setDeclare1 setDefine1 ident
checkExp f setDeclare1 setDefine1 (E.Unary unop expr) = checkExp f setDeclare1 setDefine1 expr
checkExp f setDeclare1 setDefine1 (E.Binary binop expr1 expr2) = (checkExp f setDeclare1 setDefine1 expr1) && (checkExp f setDeclare1 setDefine1 expr2)
checkExp f setDeclare1 setDefine1 (E.Ternary expr1 expr2 expr3) = (checkExp f setDeclare1 setDefine1 expr1) && (checkExp f setDeclare1 setDefine1 expr2) && (checkExp f setDeclare1 setDefine1 expr3)
checkExp f setDeclare1 setDefine1 (E.Call ident args) =
  let
    argValid = (all (checkExp f setDeclare1 setDefine1) args)
    funcValid = f ident
  in
    case (argValid, funcValid) of
      (False, _) -> error ("checkExp Call: arg not valid!" ++ show ident ++ " args:" ++ show args)
      (_, False) -> error ("checkExp Call: function name not found!" ++ show ident ++ " args:" ++ show args)
      (True, True) -> True
checkExp f setDeclare1 setDefine1 (E.Dot expr ident) = checkExp f setDeclare1 setDefine1 expr
checkExp f setDeclare1 setDefine1 (E.Alloc ctype) = True
checkExp f setDeclare1 setDefine1 (E.PointerStar expr) = checkExp f setDeclare1 setDefine1 expr
checkExp f setDeclare1 setDefine1 (E.AllocArray ctype expr) = checkExp f setDeclare1 setDefine1 expr
checkExp f setDeclare1 setDefine1 (E.ArrayAccess expr1 expr2) = all (checkExp f setDeclare1 setDefine1) [expr1, expr2]
checkExp f setDeclare1 setDefine1 (E.NULL) = True

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

