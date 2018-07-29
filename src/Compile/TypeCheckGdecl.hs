module Compile.TypeCheckGdecl (typifyGdeclList, insertScrambleMapIntoState,buildfdefsetheader,buildfdefsetsrc) where
import qualified Compile.MapWrap as MapWrap
import Compile.TypeCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tuple
import Data.Maybe 
import Compile.Constants

import qualified Compile.Types.EAST as E
import qualified Compile.Types.TAST as T
import Compile.Types
import Debug.Trace
-- ScrambleMap is a map from scrambled function name to original function name
type ScrambleMap = Map.Map Ident Ident
-- FuncMap[functionName] gives a tuple of the function's return type and the arguments
type FuncMap = Map.Map Ident (T.CType, [(T.CType, T.Ident)]) -- All CTypes here are guaranteed to be basic
-- StructMap[structName] gives you the field list of the struct
type StructMap = Map.Map Ident (Map.Map Ident T.CType)

type StructUseMap = Map.Map Ident (Set.Set Ident)
type FdefSet = Set.Set Ident


type CheckState = (FuncMap, StructMap, ScrambleMap, StructUseMap, FdefSet)

-- Fuck function name shadowing. If it weren't for that, we won't need this scrambleMap shit
-- Insert shit (scramble map) into CheckState
insertScrambleMapIntoState :: CheckState -> ScrambleMap -> CheckState
insertScrambleMapIntoState state@(funcmap,structmap, scramblemap, structusemap,fdefset) newScrambleMap =
  (funcmap,structmap, newScrambleMap, structusemap,fdefset)

-- insertFdefSetIntoState :: CheckState -> FdefSet -> CheckState
-- insertFdefSetIntoState state@(funcmap,structmap, scramblemap, structusemap,fdefset) newFdefSet =
--   (funcmap,structmap, newScrambleMap, structusemap,newFdefSet)

buildfdefsetheader :: CheckState -> E.Gdecl -> CheckState
buildfdefsetheader state@(funcmap,structmap, scramblemap, structusemap,fdefset) (E.Fdecl ctype ident _) =
	(funcmap,structmap, scramblemap, structusemap, Set.insert ident fdefset)
buildfdefsetheader state _ = state

buildfdefsetsrc :: CheckState -> E.Gdecl -> CheckState
buildfdefsetsrc state@(funcmap,structmap, scramblemap, structusemap,fdefset) (E.Fdefn _ ident _ _) =
	(funcmap,structmap, scramblemap, structusemap, Set.insert ident fdefset)
buildfdefsetsrc state _ = state

-- Typechecks and adds types to Gdecl in the Gdecl list
typifyGdeclList :: CheckState -> [E.Gdecl] -> (CheckState, [T.Gdecl])
typifyGdeclList state (gdecl:gdeclList) =
	let
		(newstate, tgdecl) = typifyGdecl state gdecl
		(finalstate, finaltgdecl) = typifyGdeclList newstate gdeclList
	in
		(finalstate, tgdecl:finaltgdecl)
typifyGdeclList state [] = (state, [])

-- Typechecks and converts Gdecl from EAST to TAST type
typifyGdecl :: CheckState -> E.Gdecl -> (CheckState, T.Gdecl)
typifyGdecl state@(funcmap,structmap, scramblemap, structusemap, fdefset) (E.Fdecl ctype ident paramlist) =
	let
		-- Not much going on here, we're just transforming things into the right types
		tparamlist = paramListToTParamList paramlist
		ttype = eastTypeToTastType ctype
		arglist = map (\(T.Param ctype ident) -> (ctype, ident)) tparamlist
		-- Insert our function declaration
		newFuncMap = Map.insert ident (ttype, arglist) funcmap
		-- And insert it into the new state
		newState = (newFuncMap, structmap, scramblemap, structusemap, fdefset)
	in
		(newState, T.Fdecl ttype ident tparamlist)
typifyGdecl state@(funcmap,structmap, scramblemap, structusemap, fdefset) (E.Fdefn ctype ident paramlist block) =
	let
		-- Not much going on here, we're just transforming things into the right types
		tparamlist = paramListToTParamList paramlist
		ttype = eastTypeToTastType ctype
		arglist = map (\(T.Param ctype ident) -> (ctype, ident)) tparamlist
		-- Insert our function declaration
		newFuncMap = Map.insert ident (ttype, arglist) funcmap
		newState = (newFuncMap, structmap, scramblemap, structusemap, fdefset)
		gdeclState = (newFuncMap, structmap, scramblemap, fdefset)
		-- checkFunction typechecks and converts our function definition from EAST to TAST
		-- It is located in TypeCheck.hs
		newGdecl = checkFunction gdeclState (E.Fdefn ctype ident paramlist block)
	in
		(newState, newGdecl)

typifyGdecl state (E.Typedef ctype ident) =
	(state, T.Typedef (eastTypeToTastType ctype) ident)
typifyGdecl state (E.Sdecl ident) = (state, T.Sdecl ident)
typifyGdecl state@(funcmap,structmap, scramblemap, structusemap, fdefset) (E.Sdef ident fieldlist) =
	let
		tfieldlist = map eastToTastField fieldlist
		tfieldtuplelist = map (\(T.Field ctype ident) -> (ctype, ident)) tfieldlist
		tfieldmap = Map.fromList (map swap tfieldtuplelist)
		-- Insert struct information into the map so checkFunction can use it
		newStructMap = Map.insert ident tfieldmap structmap
		(seen, newStructUseMap) = dfsStruct newStructMap (Set.empty, structusemap) ident
		newState = (funcmap, newStructMap, scramblemap, newStructUseMap, fdefset)
	in
		case Set.member ident seen of
			True -> (newState, T.Sdef ident tfieldlist)
			False -> error "Bug"

-- Converts EAST to TAST field. Not much going on here
eastToTastField (E.Field ctype ident) = T.Field (eastTypeToTastType ctype) ident



isStruct (T.CStruct _) = True
isStruct _ = False



-- -- Returns true iff there is a field type which is the same as the original struct
-- recursiveCheckStruct :: StructMap -> T.Ident -> T.Gdecl -> Bool
-- recursiveCheckStruct structmap originalStructName (T.Sdef ident tfieldlist) =
-- 	let
-- 		fieldtypelist = map (\(T.Field ctype ident) -> ctype) tfieldlist
-- 	in
-- 		any (recursiveCheckFieldType structmap originalStructName) fieldtypelist
-- 
-- -- Returns true iff there is a field type which is the same as the original struct
-- recursiveCheckFieldType :: StructMap -> T.Ident -> T.CType -> Bool
-- recursiveCheckFieldType structmap originalStructName (CStruct ident) =
-- 	let
-- 		tfieldmap :: (Map.Map Ident T.CType)
-- 		tfieldmap = structmap Map.! ident
-- 		fieldtypelist :: [T.CType]
-- 		fieldtypelist = Map.elems tfieldmap
-- 	in
-- 		case (ident == originalStructName) of
-- 			True -> True
-- 			False -> any (recursiveCheckFieldType structmap originalStructName) fieldtypelist
-- 
-- recursiveCheckFieldType structmap originalStructName _ = False


dfsStruct :: StructMap -> (Set.Set T.Ident, StructUseMap) -> T.Ident -> (Set.Set T.Ident, StructUseMap)
dfsStruct graph (inseen, dp) vertex =
	case (Set.member vertex inseen) of
		True -> error ("Loop in dfs struct! Recursive struct nesting detected.")
		False ->(
			let
				seen = Set.insert vertex inseen
			in
				case (Map.member vertex dp) of
					True -> (Set.union (dp Map.! vertex) seen, dp)
					False -> (
						let
							neighbors = filter isStruct (Map.elems (graph Map.! vertex))
							neighboridents = Set.toList (Set.fromList (map (\(CStruct ident) -> ident) neighbors))
							(finalSeen, newDp) = foldl (dfsStruct graph) (seen,dp) neighboridents
							(childSeen, _) = foldl (dfsStruct graph) (Set.empty, newDp) neighboridents
							finalDp = Map.insert vertex childSeen newDp
						in
							(finalSeen, finalDp)
						)
					)



						


