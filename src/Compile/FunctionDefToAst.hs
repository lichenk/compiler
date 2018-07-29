module Compile.FunctionDefToAst(functionDefToAst) where
import qualified Data.Map as Map

import qualified Compile.Types.EAST as E
import qualified Compile.Types.FAST as F
type FuncMap = Map.Map F.Ident ([(F.CType, F.Ident)], Maybe E.AST , F.CType) -- All F.CTypes here are guaranteed to be basic
functionDefToAst :: FuncMap -> E.AST
functionDefToAst funcmap = 
	let
		(_, Just ast, _) = (Map.!) funcmap "main"
	in
		ast