module Compile.MangleFunctionName (mangleFuncMap) where
import Compile.Types.EAST
import Compile.GlobalTypeFind
import Compile.Constants
import qualified Data.Map as Map


mangleFuncName :: FunctionDeclMap -> Ident -> Ident
mangleFuncName headerSet funcName =
  case (funcName, Set.member funcName headerSet) of
    ("main", _) -> functionPrefix ++ funcName
    (_, True) -> headerPrefix ++ funcName --Is header
    (_, False) -> functionPrefix ++ funcName  -- Is source

mangleAst :: FunctionDeclMap -> AST -> AST
mangleAst headerSet (Func block) = Func (mangleBlock headerSet block)
mangleBlock :: FunctionDeclMap -> Block -> Block
mangleBlock headerSet block = map (mangleStmt headerSet) block
mangleStmt :: FunctionDeclMap -> Stmt -> Stmt
mangleStmt headerSet (Simp simp) = Simp (mangleSimp headerSet simp)
mangleStmt headerSet (Ctrl ctrl) =  Ctrl (mangleCtrl headerSet ctrl)
mangleStmt headerSet (Blk block) =  Blk (mangleBlock headerSet block)
mangleSimp :: FunctionDeclMap -> Simp -> Simp
mangleSimp headerSet (Exp expr) = Exp (mangleExp headerSet expr)
mangleSimp headerSet (Asgn lvalue expr) = Asgn lvalue (mangleExp headerSet expr)
mangleSimp headerSet other = other
mangleCtrl :: FunctionDeclMap -> Ctrl -> Ctrl
mangleCtrl headerSet (If expr stmt1 stmt2) = If (mangleExp headerSet expr) (mangleStmt headerSet stmt1) (mangleStmt headerSet stmt2)
mangleCtrl headerSet (While expr stmt1) = While (mangleExp headerSet expr) (mangleStmt headerSet stmt1)
mangleCtrl headerSet (Ret expr) = Ret (mangleExp headerSet expr)
mangleCtrl headerSet RetVoid = RetVoid

mangleExp :: FunctionDeclMap -> Exp ->Exp
mangleExp headerSet (Call ident arglist) = Call (mangleFuncName headerSet ident) (map (mangleExp headerSet) arglist)
mangleExp headerSet (Unary unop expr) = Unary unop (mangleExp headerSet expr)
mangleExp headerSet (Binary binop expr1 expr2) = Binary binop (mangleExp headerSet expr1) (mangleExp headerSet expr2)
mangleExp headerSet (Ternary expr1 expr2 expr3) = Ternary (mangleExp headerSet expr1) (mangleExp headerSet expr2) (mangleExp headerSet expr3)
mangleExp headerSet other = other
