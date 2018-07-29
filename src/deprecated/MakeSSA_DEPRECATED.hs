module Compile.MakeSSA where

import Text.ParserCombinators.Parsec.Pos (SourcePos)
import Numeric as Num
import Compile.Types.Ops
import Compile.Types.AST
import qualified Data.Map as Map

makeSSA :: AST -> AST
makeSSA ast =
  let
    variable_table = Map.empty
    newstmts = []
    Block stmts start = ast
  in

-- Takes the current variable table and a statement, and returns the updated
-- table and the updated version of that statement
updateStatement :: Map.Map String Int -> Stmt -> (Map.Map String Int, Stmt)
updateStatement variable_table (Decl dec) =

-- Takes the current variable table and an expression, and returns the updated
-- expression where all variables have their updated versions
updateExpression :: Map.Map String Int -> Exp -> Exp
updateExpression t (Ident v pos) =
  Ident (v ++ " " ++  (show (Map.findWithDefault 0 v t))) pos
updateExpression t (Binop b e1 e2 pos) =
  Binop b (updateExpression t e1) (updateExpression t e2) pos
updateExpression t (Unop u e pos) =
  Unop u (updateExpression e) pos