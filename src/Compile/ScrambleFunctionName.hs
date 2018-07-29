-- Scramble scrambles all function names except int main() from the source header file
-- This is so that shit like abort() can be redefined.
-- Scramble should ONLY be called on the source file!! Not the header!

module Compile.ScrambleFunctionName (scrambleAst, getScrambleMap) where
import qualified Compile.Types.FAST as F
import Compile.Constants
import qualified Data.Set as Set
import qualified Data.Map as Map
type HeaderFunctionSet = Set.Set F.Ident

-- ScrambleMap is a map from scrambled function name to original function name
type ScrambleMap = Map.Map F.Ident F.Ident

predefinedFunctions = [("abort","abort")]

getScrambleMap :: HeaderFunctionSet -> [F.Gdecl] -> ScrambleMap
getScrambleMap headerSet gdeclList =
  let
    headerFunctionList = (Set.toList headerSet)
    headerScrambleList = map (scrambleFunctionToTuple headerSet) headerFunctionList
    headerScrambleMap = Map.fromList (predefinedFunctions ++ headerScrambleList)
    fullScrambleMap = foldl (insertScrambleMap headerSet) headerScrambleMap (predeclaredFunctions ++ gdeclList)
  in
    fullScrambleMap

-- Takes function and returns (scrambledfunction, functionname)
scrambleFunctionToTuple :: HeaderFunctionSet -> F.Ident -> (F.Ident, F.Ident)
scrambleFunctionToTuple headerSet function = (scrambleFuncName headerSet function, function)


insertScrambleMap :: HeaderFunctionSet -> ScrambleMap -> F.Gdecl -> ScrambleMap
insertScrambleMap headerSet scrambleMap (F.Fdecl ctype ident paramlist) =
  Map.insert (scrambleFuncName headerSet ident) ident scrambleMap
insertScrambleMap headerSet scrambleMap (F.Fdefn ctype ident paramlist block) =
  Map.insert (scrambleFuncName headerSet ident) ident scrambleMap
insertScrambleMap headerSet scrambleMap _ = scrambleMap


scrambleFuncName :: HeaderFunctionSet -> F.Ident -> F.Ident
scrambleFuncName headerSet funcName =
  case (funcName, Set.member funcName headerSet) of
    -- main should always remain as main. _c0 prefix will be added by MangleFunctionName.hs
    ("main",_) -> functionPrefix ++ funcName
    -- abort can be redefined by source. Scramble it.
    ("abort",_) -> scramblePrefix ++ funcName
    -- Don't scramble header file functions
    (_, True) -> funcName
    (_, False) -> functionPrefix ++ funcName

scrambleAst :: HeaderFunctionSet -> [F.Gdecl] -> [F.Gdecl]
scrambleAst headerSet ast = map (scrambleGdecl headerSet) ast

scrambleGdecl :: HeaderFunctionSet -> F.Gdecl -> F.Gdecl
scrambleGdecl headerSet (F.Fdecl ctype ident paramlist) =
  F.Fdecl ctype (scrambleFuncName headerSet ident) paramlist
scrambleGdecl headerSet (F.Fdefn ctype ident paramlist block) =
  F.Fdefn ctype (scrambleFuncName headerSet ident) paramlist (scrambleBlock headerSet block)
scrambleGdecl headerSet (F.Typedef ctype ident) = F.Typedef ctype ident
scrambleGdecl headerSet (F.Sdecl ident) = (F.Sdecl ident)
scrambleGdecl headerSet (F.Sdef ident fieldlist) = (F.Sdef ident fieldlist)

scrambleBlock :: HeaderFunctionSet -> F.Block -> F.Block
scrambleBlock headerSet block = map (scrambleStmt headerSet) block
scrambleStmt :: HeaderFunctionSet -> F.Stmt -> F.Stmt
scrambleStmt headerSet (F.Simp simp) =  F.Simp (scrambleSimp headerSet simp)
scrambleStmt headerSet (F.Ctrl ctrl) =  F.Ctrl (scrambleCtrl headerSet ctrl)
scrambleStmt headerSet (F.Blk block) =  F.Blk (scrambleBlock headerSet block)
scrambleSimp :: HeaderFunctionSet -> F.Simp -> F.Simp
scrambleSimp headerSet (F.Exp expr) = F.Exp (scrambleExp headerSet expr)
scrambleSimp headerSet (F.Asgn lvalue asnop expr) = F.Asgn (scrambleLvalue headerSet lvalue) asnop (scrambleExp headerSet expr)
scrambleSimp headerSet (F.Decl decl) = F.Decl (scrambleDecl headerSet decl)
scrambleSimp headerSet (F.Post lvalue postop) = F.Post (scrambleLvalue headerSet lvalue) postop
scrambleSimp headerSet other = other
scrambleCtrl :: HeaderFunctionSet -> F.Ctrl -> F.Ctrl
scrambleCtrl headerSet (F.If expr stmt1 elseopt) = 
  F.If (scrambleExp headerSet expr) (scrambleStmt headerSet stmt1) (scrambleElseopt headerSet elseopt)
scrambleCtrl headerSet (F.While expr stmt1) =
  F.While (scrambleExp headerSet expr) (scrambleStmt headerSet stmt1)
scrambleCtrl headerSet (F.For simpopt1 expr simpopt2 stmt) =
  F.For (scrambleSimpopt headerSet simpopt1) (scrambleExp headerSet expr) (scrambleSimpopt headerSet simpopt2) (scrambleStmt headerSet stmt)
scrambleCtrl headerSet (F.Ret expr) = F.Ret (scrambleExp headerSet expr)
scrambleCtrl headerSet F.RetVoid = F.RetVoid
scrambleCtrl headerSet (F.Assert expr) = F.Assert (scrambleExp headerSet expr)

scrambleLvalue headerSet (F.Variable ident) = (F.Variable ident)
scrambleLvalue headerSet (F.LvalueDot lvalue ident) = (F.LvalueDot (scrambleLvalue headerSet lvalue) ident)
scrambleLvalue headerSet (F.LvalueArrow lvalue ident) = (F.LvalueArrow (scrambleLvalue headerSet lvalue) ident)
scrambleLvalue headerSet (F.LvaluePointerStar lvalue) = (F.LvaluePointerStar (scrambleLvalue headerSet lvalue))
scrambleLvalue headerSet (F.LvalueArrayAccess lvalue expr) = (F.LvalueArrayAccess (scrambleLvalue headerSet lvalue) (scrambleExp headerSet expr))

scrambleExp :: HeaderFunctionSet -> F.Exp -> F.Exp
scrambleExp headerSet (F.Const n) = (F.Const n)
scrambleExp headerSet (F.CTrue ) = (F.CTrue )
scrambleExp headerSet (F.CFalse ) = (F.CFalse )
scrambleExp headerSet (F.Var ident ) = (F.Var ident )
scrambleExp headerSet (F.Call ident arglist) = F.Call (scrambleFuncName headerSet ident) (map (scrambleExp headerSet) arglist)
scrambleExp headerSet (F.Unary unop expr) = F.Unary unop (scrambleExp headerSet expr)
scrambleExp headerSet (F.Binary binop expr1 expr2) = F.Binary binop (scrambleExp headerSet expr1) (scrambleExp headerSet expr2)
scrambleExp headerSet (F.Ternary expr1 expr2 expr3) = F.Ternary (scrambleExp headerSet expr1) (scrambleExp headerSet expr2) (scrambleExp headerSet expr3)
scrambleExp headerSet (F.Dot expr ident) = (F.Dot (scrambleExp headerSet expr) ident)
scrambleExp headerSet (F.Arrow expr ident) = (F.Arrow (scrambleExp headerSet expr) ident)
scrambleExp headerSet (F.Alloc ctype) = (F.Alloc ctype)
scrambleExp headerSet (F.PointerStar expr) = (F.PointerStar (scrambleExp headerSet expr))
scrambleExp headerSet (F.AllocArray ctype expr) = (F.AllocArray ctype (scrambleExp headerSet expr))
scrambleExp headerSet (F.ArrayAccess expr1 expr2) = (F.ArrayAccess (scrambleExp headerSet expr1) (scrambleExp headerSet expr2))
scrambleExp headerSet (F.NULL) = (F.NULL)

scrambleDecl :: HeaderFunctionSet -> F.Decl -> F.Decl
scrambleDecl headerSet (F.JustDecl ctype ident) = F.JustDecl ctype ident
scrambleDecl headerSet (F.DeclAsgn ctype ident expr) = F.DeclAsgn ctype ident (scrambleExp headerSet expr)

scrambleSimpopt :: HeaderFunctionSet -> F.Simpopt -> F.Simpopt
scrambleSimpopt headerSet F.Epsilon = F.Epsilon
scrambleSimpopt headerSet (F.Simpopt simp) = F.Simpopt (scrambleSimp headerSet simp)

scrambleElseopt :: HeaderFunctionSet -> F.Elseopt -> F.Elseopt
scrambleElseopt headerSet F.Eps = F.Eps
scrambleElseopt headerSet (F.Else stmt) = F.Else (scrambleStmt headerSet stmt)

