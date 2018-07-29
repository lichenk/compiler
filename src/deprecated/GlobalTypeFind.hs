module Compile.GlobalTypeFind(convertFuncMap, makeTypeAstMaps,convertDeclToDefinitionMap,TypedefMap,FunctionDeclMap,FunctionDefineMap) where
import qualified Compile.MapWrap as MapWrap
import Compile.Types.FAST as F
import Compile.Types.EAST as E
import Compile.Elaboration
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Compile.TypeCheck as TypeCheck
import qualified Compile.ScopeCheck as ScopeCheck
import Data.Maybe
import Debug.Trace

-- TypedefMap[ident] returns the C type of the ident as defined by previous typedefs
type TypedefMap = Map.Map F.Ident F.CType

-- FunctionDeclMap[f] returns (ReturnType, [Argument Type]) where ReturnType is the return value of the function f, and [Argument Type] is the list of types
-- of the arguments of f
type FunctionDeclMap = Map.Map F.Ident (F.CType, [F.CType])

-- FunctionDefinitionMap[f] returns (ReturnType, [F.Param]) where ReturnType is the return value of the function f, and [F.Param] is the list of
-- the parameters of f, where F.Param is defined as (F.Param F.CType F.Ident) in FF.AST.hs. F.CType is the type of the parameter, and F.Ident is the name of the parameter.
type FunctionDefineMap = Map.Map F.Ident (F.CType, [F.Param], Maybe E.AST)

-- These are the types taken in by typechecker and scopechecker
type FuncMap = Map.Map F.Ident ([(F.CType, F.Ident)], Maybe E.AST , F.CType) -- All F.CTypes here are guaranteed to be basic
type TypeMap = Map.Map F.Ident F.CType
type FuncCheck = [F.CType] -> F.Ident -> (Bool, F.CType)

-- Convert into the form accepted by typechecker
convertFunctionDefinition::(F.CType, [F.Param], Maybe E.AST) -> ([(F.CType, F.Ident)], Maybe E.AST, F.CType)
convertFunctionDefinition (ctype, paramlist, maybeast) =
	(
		map (\(F.Param ctype ident) -> (ctype,ident)) paramlist,
		maybeast,
		ctype
	)

-- Convert into the form accepted by typechecker
convertFuncMap::(TypedefMap, FunctionDeclMap, FunctionDefineMap) -> FuncMap
convertFuncMap (_,_, newFunctiondefmap) =
	Map.map convertFunctionDefinition newFunctiondefmap

-- Parameter with real type and fake name for typechecker
typeToParam :: F.CType -> F.Param
typeToParam ctype = F.Param ctype "header_function_arg"

-- This function is for header files
-- Why convert declarations to definitions in header files?
-- Reason 1: Header file declarations are also considered as "defined"
-- Reason 2: Header file declarations must be passed to the typechecker in the Function Map
convertDeclToDefinition :: (F.CType, [F.CType]) -> (F.CType, [F.Param], Maybe E.AST)
convertDeclToDefinition (returnType, argTypeList) = (returnType, map typeToParam argTypeList, Nothing)

-- This function is for header files
-- Why convert declarations to definitions in header files?
-- Reason 1: Header file declarations are also considered as "defined"
-- Reason 2: Header file declarations must be passed to the typechecker in the Function Map
convertDeclToDefinitionMap::FunctionDeclMap -> FunctionDefineMap
convertDeclToDefinitionMap functiondefmap =
	Map.map convertDeclToDefinition functiondefmap

-- Input: Old (TypedefMap, FunctionDeclMap, FunctionDefineMap) (presumably) supplied by a previous header file
-- TypedefMap[ident] returns the C type of the ident as defined by previous typedefs
-- FunctionDeclMap[f] returns (ReturnType, [Argument Type]) where ReturnType is the return value of the function f, and [Argument Type] is the list of types
-- of the arguments of f
-- FunctionDefinitionMap[f] returns (ReturnType, [F.Param]) where ReturnType is the return value of the function f, and [F.Param] is the list of
-- the parameters of f, where F.Param is defined as (F.Param F.CType F.Ident) in AST.hs. F.CType is the type of the parameter, and F.Ident is the name of the parameter.
-- Output: New (TypedefMap, FunctionDeclMap, FunctionDefineMap) after looking at the current file
makeTypeAstMaps::(TypedefMap, FunctionDeclMap, FunctionDefineMap) -> F.AST -> ((TypedefMap, FunctionDeclMap, FunctionDefineMap), FuncMap)
makeTypeAstMaps input@(typedefmap1, functiondeclmap1, definedmap1) ast =
	foldl typefindGdecl ((typedefmap1, functiondeclmap1, definedmap1), (convertFuncMap input)) ast

-- Gdecl stands for Global Declaration
typefindGdecl::((TypedefMap, FunctionDeclMap, FunctionDefineMap), FuncMap) -> F.Gdecl -> ((TypedefMap, FunctionDeclMap, FunctionDefineMap), FuncMap)
typefindGdecl ((typedefmap1, functiondeclmap1, definedmap1), typecheckMap) (Fdecl ctype1 ident paramlist1) =
	let
		convertedParamList = convertParamList typedefmap1 paramlist1
		paramidentlist = map (\(F.Param _ ident) -> ident) convertedParamList
		hasDuplicates = (Set.size (Set.fromList paramidentlist)) < (length paramidentlist)
		hasTypeNameParam = any (\t -> Map.member t typedefmap1) paramidentlist
		paramtypelist = map (\(F.Param ctype ident) -> ctype) convertedParamList
		convertedCtype = convertType typedefmap1 ctype1
		-- Does the function have a parameter that is void?
		hasVoidParam = any (\t -> (t == F.CNoType) || (t == F.CVoid)) paramtypelist
		-- isDefinedAsTypedef: Is ident declared as a function previously?
		isDeclaredAsFunction = Map.member ident functiondeclmap1
		-- isDefinedAsTypedef: Is ident defined as a typedef?
		isDefinedAsTypedef = Map.member ident typedefmap1
		-- isCompatbile: Whether this declaration matches the previous declaration
		isCompatible = (((MapWrap.!) functiondeclmap1 ident) == (convertedCtype, paramtypelist))
		-- New function declaration map including the current declaration
		functiondeclmap2 = Map.insert ident (convertedCtype, paramtypelist) functiondeclmap1
		typecheckMap2 = 
			if Map.member ident typecheckMap
			then typecheckMap
			else Map.insert ident (convertFunctionDefinition (convertedCtype, convertedParamList, Nothing)) typecheckMap
	in
		case (hasVoidParam || hasDuplicates || hasTypeNameParam, isDefinedAsTypedef, isDeclaredAsFunction) of
			(True, _, _) -> error("Functions can't have a void parameter or duplicate parameters or typename parameters")
			(_, True, _) -> error("F.Ident used as function name was previously defined in a typedef: " ++ show ident ++ " in context:" ++ show (Fdecl ctype1 ident paramlist1))
			(_, False, True) -> (
				case isCompatible of
					True -> ((typedefmap1, functiondeclmap2, definedmap1), typecheckMap2)
					False -> error("Function declaration is incompatible with previous declaration:" ++ show (Fdecl ctype1 ident paramlist1))
				)
			(_, False, False) -> ((typedefmap1, functiondeclmap2, definedmap1), typecheckMap2)

typefindGdecl ((typedefmap1, functiondeclmap1, definedmap1), typecheckMap) (F.Fdefn ctype1 ident paramlist1 block) =
	let
		(_, _, potentialAst) = Map.findWithDefault (F.CNoType, [], Nothing) ident definedmap1
		isDefined = isJust potentialAst
		convertedParamlist = convertParamList typedefmap1 paramlist1
		paramidentlist = map (\(F.Param _ ident) -> ident) convertedParamlist
		hasTypeNameParam = any (\t -> Map.member t typedefmap1) paramidentlist
		hasDuplicates = (Set.size (Set.fromList paramidentlist)) < (length paramidentlist)
		paramtypelist = map (\(F.Param ctype ident) -> ctype) convertedParamlist
		-- Does the function have a parameter that is void?
		hasVoidParam = any (\t -> (t == F.CNoType) || (t == F.CVoid)) paramtypelist
		convertedCtype =convertType typedefmap1 ctype1
		((typedefmap2, functiondeclmap2, definedmap2), typecheckMap2) = typefindGdecl ((typedefmap1, functiondeclmap1, definedmap1), typecheckMap) (Fdecl convertedCtype ident convertedParamlist)
		-- New function definition map including the current definition
		-- type FunctionDefineMap = Map.Map F.Ident (F.CType, [F.Param], F.AST)
		elaborated = elabBlock block (typedefmap2, functiondeclmap2)
		justast = Just (E.Func (elaborated))
		-- here I should use typecheckMap2 to scope and typecheck the ast
		definedmap3 = Map.insert ident (convertedCtype, convertedParamlist, justast) definedmap2
		typecheckMap3 = Map.insert ident (convertFunctionDefinition (convertedCtype, convertedParamlist, justast)) typecheckMap2
	in
		case (hasVoidParam || hasDuplicates || hasTypeNameParam, isDefined) of
			(True, _) -> error ("Functions may not have void parameters or duplicate parameters or typename parameter")
			(_, True) -> error("Function should not be redefined:" ++ show (F.Fdefn ctype1 ident paramlist1 block))
			(_, False) -> 
				if ((ScopeCheck.checkFunction typecheckMap3 ident) && (TypeCheck.checkFunction typecheckMap3 ident))
				then ((typedefmap2, functiondeclmap2, definedmap3), typecheckMap3)
				else error ("Scopechecking or Typechecking failed on " ++ ident)
			

typefindGdecl ((typedefmap1, functiondeclmap1, definedmap1), typecheckMap) (F.Typedef ctype1 ident) =
	let
		isDefined = Map.member ident typedefmap1
		isDeclaredAsFunction = Map.member ident functiondeclmap1
		convertedCtype = convertType typedefmap1 ctype1
		-- New typedef map including the current typedef
		typedefmap2 = Map.insert ident convertedCtype typedefmap1
	in
		case (convertedCtype, isDefined, isDeclaredAsFunction) of
			(F.CVoid, _, _) -> error("Void cannot be used in a typedef")
			(_, True,_) -> error("F.Typedef identifier previously defined as typedef:" ++ show (F.Typedef ctype1 ident))
			(_, _,True) -> error("F.Typedef identifier previously defined as function:" ++ show (F.Typedef ctype1 ident))
			(_, False, False) -> ((typedefmap2, functiondeclmap1, definedmap1), typecheckMap)


convertType :: TypedefMap -> F.CType -> F.CType
convertType typedefmap1 (F.CTypeIdent ident) =
	case (Map.member ident typedefmap1) of
		True -> ((MapWrap.!) typedefmap1 ident)
		False -> error("F.Identifier being used as a type, but was not defined as a type:" ++ show ident)
convertType typedefmap1 F.CInt = F.CInt
convertType typedefmap1 F.CBool = F.CBool
convertType typedefmap1 F.CVoid = F.CVoid

convertParam :: TypedefMap -> F.Param -> F.Param
convertParam typedefmap1 (F.Param ctype ident) = (F.Param (convertType typedefmap1 ctype) ident)

convertParamList :: TypedefMap -> [F.Param] -> [F.Param]
convertParamList typedefmap1 paramlist = map (convertParam typedefmap1) paramlist

{-
untypedefF.Block :: TypedefMap -> F.Block -> F.Block
untypedefF.Block typedefmap block = map (untypedefStmt typedefmap) block

untypedefDecl :: TypedefMap -> Decl -> Decl
untypedefDecl typedefmap (JustDecl ctype ident) = JustDecl (convertType typedefmap ctype) ident
untypedefDecl typedefmap (DeclAsgn ctype ident expr) = DeclAsgn (convertType typedefmap ctype) ident (untypedefExp typedefmap expr)

untypedefStmt :: TypedefMap -> Stmt -> Stmt
untypedefStmt typedefmap (Simp simp) = Simp (untypedefSimp typedefmap simp)
untypedefStmt typedefmap (Ctrl ctrl) = Ctrl (untypedefCtrl typedefmap ctrl)
untypedefStmt typedefmap (Blk block) = Blk (untypedefF.Block typedefmap block)

untypedefSimp :: TypedefMap -> Simp -> Simp
untypedefSimp typedefmap (Asgn lvalue asnop expr) = (Asgn lvalue asnop (untypedefExp typedefmap expr))
untypedefSimp typedefmap (Post lvalue postop) = (Post lvalue postop)
untypedefSimp typedefmap (Decl decl) = Decl (untypedefDecl typedefmap decl)
untypedefSimp typedefmap (Exp expr) = Exp (untypedefExp typedefmap expr)

untypedefSimpopt :: TypedefMap -> Simpopt -> Simpopt
untypedefSimpopt typedefmap Epsilon = Epsilon
untypedefSimpopt typedefmap (Simpopt simp) = Simpopt (untypedefSimp typedefmap simp)

untypedefElseopt :: TypedefMap -> Elseopt -> Elseopt
untypedefElseopt typedefmap Eps = Eps
untypedefElseopt typedefmap (Else stmt) = Else (untypedefStmt typedefmap stmt)

untypedefCtrl :: TypedefMap -> Ctrl -> Ctrl
untypedefCtrl typedefmap (If expr stmt elseopt) = If (untypedefExp typedefmap expr) (untypedefStmt typedefmap stmt) (untypedefElseopt typedefmap elseopt)
untypedefCtrl typedefmap (While expr stmt) = While (untypedefExp typedefmap expr) (untypedefStmt typedefmap stmt)
untypedefCtrl typedefmap (Ret expr) = Ret (untypedefExp typedefmap expr)
untypedefCtrl typedefmap (RetVoid) = RetVoid
untypedefCtrl typedefmap (Assert expr) = Assert (untypedefExp typedefmap expr)

untypedefExp :: TypedefMap -> Exp -> Exp
untypedefExp typedefmap (Const n) = Const n
untypedefExp typedefmap (CTrue) = CTrue
untypedefExp typedefmap (CFalse) = CFalse
untypedefExp typedefmap (Var ident) = Var ident
untypedefExp typedefmap (Unary unop expr) = Unary unop (untypedefExp typedefmap expr)
untypedefExp typedefmap (Binary binop expr1 expr2) = Binary binop (untypedefExp typedefmap expr1) (untypedefExp typedefmap expr2)
untypedefExp typedefmap (Ternary expr1 expr2 expr3) = Ternary (untypedefExp typedefmap expr1) (untypedefExp typedefmap expr2) (untypedefExp typedefmap expr3)
untypedefExp typedefmap (Call ident exprlist) = Call ident (map (untypedefExp typedefmap) exprlist)
-}
