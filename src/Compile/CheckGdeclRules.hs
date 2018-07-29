-- Enforces the rules that the following are errors:
-- Functions can't have a void parameter or duplicate parameters or typename parameters"
-- E.Ident can't be used as function name was previously defined in a typedef
-- Function declaration is incompatible with previous declaration
-- Function should not be redefined
-- Void cannot be used in a typedef
-- E.Typedef identifier previously defined as typedef
-- E.Typedef identifier previously defined as function
-- E.Identifier being used as a type, but was not defined as a type (outside of a block)

module Compile.CheckGdeclRules(checkGdeclList, insertScrambleMapIntoCheckStateAndMarkSource) where
import qualified Compile.MapWrap as MapWrap
import Compile.Types.EAST as E
import Compile.Elaboration
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.Tuple
import Debug.Trace


type FdefSet = Set.Set E.Ident
-- FunctionDeclMap[f] returns (ReturnType, [Argument Type]) where ReturnType is the return value of the function f, and [Argument Type] is the list of types
-- of the arguments of f
type FdeclMap = Map.Map E.Ident (E.CType, [E.CType])
type TypeSet = Set.Set E.Ident
type StructSet = Set.Set E.Ident
-- ScrambleMap is a map from scrambled function name to original function name
-- It should really be called inverse scramble map
type ScrambleMap = Map.Map E.Ident E.Ident
type FwdScrambleMap = Map.Map E.Ident E.Ident
type IsHeader = Bool
type CheckState = (TypeSet, FdeclMap, FdefSet, StructSet, ScrambleMap, IsHeader, FwdScrambleMap)

insertScrambleMapIntoCheckStateAndMarkSource scramblemap (typeset, fdeclmap, fdefset, structset, oldscramblemap, isheader, oldfwdscramblemap) =
	let
		-- Compute inverse of scramblemap
		fwdscramblemap = Map.fromList (map swap (Map.toList scramblemap))
	in
		(typeset, fdeclmap, fdefset, structset, scramblemap, False, fwdscramblemap)


-- Glorified foldl
checkGdeclList :: CheckState -> [E.Gdecl] -> (CheckState, Bool)
checkGdeclList checkstate (gdecl:gdeclList) =
	let
		(newCheckstate, newGood) = checkGdecl checkstate gdecl
		(finalCheckState, finalGood) = checkGdeclList newCheckstate gdeclList
	in
		(finalCheckState, newGood && finalGood)

checkGdeclList checkstate [] = (checkstate, True)

-- Checks Gdecls for scoping
checkGdecl :: CheckState -> E.Gdecl -> (CheckState, Bool)
checkGdecl (typeset, fdeclmap, fdefset, structset, scramblemap, isheader, fwdscramblemap) (E.Fdecl ctype ident paramlist) =
	let
		paramidentlist = map (\(E.Param _ ident) -> ident) paramlist
		hasDuplicates = (Set.size (Set.fromList paramidentlist)) < (length paramidentlist)
		hasTypeNameParam = any (\t -> Set.member t typeset) paramidentlist
		paramtypelist = map (\(E.Param ctype ident) -> ctype) paramlist
		-- Does the function have a parameter that is void?
		hasVoidParam = any (\t -> (t == E.CNoType) || (t == E.CVoid)) paramtypelist
		-- isDefinedAsFunction: Is ident declared as a function previously?
		isDeclaredAsFunction = Map.member ident fdeclmap
		-- isDefinedAsTypedef: Is ident defined as a typedef?
		realFunctionName = case (isheader, ident) of
			(True,_) -> ident
			(_, "main") -> ident
			(False,_) -> (case Map.member ident scramblemap of
				True -> scramblemap Map.! ident
				False -> error("Can't find in scramblemap:" ++ ident)
				)
		isDefinedAsTypedef = Set.member realFunctionName typeset
		-- isNotCompatbile: Whether this declaration matches the previous declaration
		isCompatible = (((Map.!) fdeclmap ident) == (ctype, paramtypelist))
		newFdeclmap = Map.insert ident (ctype, paramtypelist) fdeclmap
	in
		case (hasVoidParam || hasDuplicates || hasTypeNameParam, isDefinedAsTypedef, isDeclaredAsFunction) of
			(True, _, _) -> error("Functions can't have a void parameter or duplicate parameters or typename parameters")
			(_, True, _) -> error("E.Ident used as function name was previously defined in a typedef: " ++ show ident)
			(_, False, True) -> (
				case isCompatible of
					True -> ((typeset, newFdeclmap, fdefset, structset, scramblemap, isheader, fwdscramblemap), True)
					False -> error("Function declaration is incompatible with previous declaration:" ++ show ident)
				)
			(False, False, False) -> ((typeset, newFdeclmap, fdefset, structset, scramblemap, isheader, fwdscramblemap), True)

checkGdecl (typeset, fdeclmap, fdefset, structset, scramblemap, isheader, fwdscramblemap) (E.Fdefn ctype ident paramlist block) =
	let
		isDefined = Set.member ident fdefset
		newFdefset = Set.insert ident fdefset
	in
		case isDefined of
			True -> error("Function cannot be redefined" ++ show ident)
			False -> checkGdecl (typeset, fdeclmap, newFdefset, structset, scramblemap, isheader, fwdscramblemap) (E.Fdecl ctype ident paramlist)

checkGdecl (typeset, fdeclmap, fdefset, structset, scramblemap, isheader, fwdscramblemap) (E.Typedef ctype ident) =
	let
		isDefinedAsTypedef = Set.member ident typeset
		(scrambledFunctionName, canFindScrambledFunctionName) = case (isheader, ident) of
			(True,_) -> (ident, True)
			(_, "main") -> (ident, True)
			(False,_) -> (case Map.member ident fwdscramblemap of
				True -> (scramblemap Map.! ident, True)
				False -> ("000Can'tfindfunction000", False)
				)
		isDeclaredAsFunction = canFindScrambledFunctionName && (Map.member scrambledFunctionName fdeclmap)
		isVoid = (ctype == E.CVoid)
		-- New typedef map including the current typedef
		newTypeset = Set.insert ident typeset
	in
		case (isDefinedAsTypedef, isDeclaredAsFunction, isVoid) of
			(False, False, False) -> ((newTypeset, fdeclmap, fdefset, structset, scramblemap, isheader, fwdscramblemap), True)
			(True, _, _) -> error("Typedef cannot be redefined" ++ show ident)
			(_, True, _) -> error("Function name used as typedef ident" ++ show ident)
			(_, _, True) -> error("Cannot typedef void" ++ show ident)

-- Note: Struct names are allowed to conflict with type names and function names
checkGdecl (typeset, fdeclmap, fdefset, structset, scramblemap, isheader, fwdscramblemap) (E.Sdef ident fieldlist) =
	let
		fieldTypeList = map (\(E.Field ctype iden) -> ctype) fieldlist
		hasVoid = any (\t -> (t == E.CNoType) || (t == E.CVoid)) fieldTypeList
		isPreviouslyDefinedAsStruct = Set.member ident structset
		newStructset = Set.insert ident structset
	in
		case (hasVoid, isPreviouslyDefinedAsStruct) of
			(_, True) -> error("Sdef redefines struct")
			(True,_) -> error("Sdef contains void")
			(False,False) -> ((typeset, fdeclmap, fdefset, newStructset, scramblemap, isheader, fwdscramblemap), True)

-- Sdecl is useless, do nothing
checkGdecl (typeset, fdeclmap, fdefset, structset, scramblemap, isheader, fwdscramblemap) (E.Sdecl ident) =
	((typeset, fdeclmap, fdefset, structset, scramblemap, isheader, fwdscramblemap), True)

