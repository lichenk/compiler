module Compile.ElaborateGdeclList(elaborateGdeclList) where

import Compile.Elaboration
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Compile.Types.FAST as F
import qualified Compile.Types.EAST as E
import qualified Compile.MapWrap as MapWrap
import Debug.Trace

-- Glorified foldl wrapper for elaborateGdecl
-- Given initial ElabState (from header file) and list of Gdecl ASTs,
-- Elaborate the ASTs and return them along with the final Elab State
elaborateGdeclList :: ElabState -> [F.Gdecl] -> (ElabState, [E.Gdecl])
elaborateGdeclList elabState (gdecl:gdeclList) =
  let
    (newElabState, elaboratedGdecl) = elaborateGdecl elabState gdecl
    (finalElabState, finalElaboratedList) = elaborateGdeclList newElabState gdeclList
  in
    (finalElabState, elaboratedGdecl:finalElaboratedList)

elaborateGdeclList elabState [] = (elabState, [])

elaborateGdecl :: ElabState -> F.Gdecl -> (ElabState, E.Gdecl)
elaborateGdecl elabState (F.Fdecl ctype ident paramlist) = elaborateFdecl elabState (F.Fdecl ctype ident paramlist)
elaborateGdecl elabState (F.Fdefn ctype ident paramlist block) = elaborateFdefn elabState (F.Fdefn ctype ident paramlist block)
elaborateGdecl elabState (F.Typedef ctype ident) = elaborateTypedef elabState (F.Typedef ctype ident)
elaborateGdecl elabState (F.Sdecl ident) = elaborateSdecl elabState (F.Sdecl ident)
elaborateGdecl elabState (F.Sdef ident fieldlist) = elaborateSdef elabState (F.Sdef ident fieldlist)

elaborateFdecl :: ElabState -> F.Gdecl -> (ElabState, E.Gdecl)
elaborateFdecl elabState (F.Fdecl ctype ident paramlist) =
  let
    newCtype = lookupType elabState ctype
    newParamList = lookupParamType elabState paramlist
    newAst = E.Fdecl (fastTypeToEastType newCtype) ident (fastParamToEastParamList newParamList)
    newElabState = insertFdeclMap ident newCtype newParamList elabState
  in
    (newElabState, newAst)


elaborateFdefn :: ElabState -> F.Gdecl -> (ElabState, E.Gdecl)
elaborateFdefn elabState (F.Fdefn ctype ident paramlist block) =
  let
    newCtype = lookupType elabState ctype
    newFParamList = lookupParamType elabState paramlist
    newEParamList = fastParamToEastParamList newFParamList
    newElabState = insertFdeclMap ident newCtype newFParamList elabState
    newBlock = elabBlock block newElabState
    newAst = E.Fdefn (fastTypeToEastType newCtype) ident newEParamList newBlock
  in
    (newElabState, newAst)
-- insertFdeclMap :: F.Ident -> F.CType -> [F.Param] -> ElabState

elaborateTypedef :: ElabState -> F.Gdecl -> (ElabState, E.Gdecl)
elaborateTypedef elabState@(typeMap,fdeclMap) (F.Typedef ctype ident) = 
  let
    newCtype = lookupType elabState ctype
    newElabState = insertType (TypeDefName ident) newCtype elabState
    -- newElabState = (Map.insert ident newCtype typeMap, fdeclMap)
  in
    (newElabState, E.Typedef (fastTypeToEastType newCtype) ident)

elaborateSdef :: ElabState -> F.Gdecl -> (ElabState, E.Gdecl)
elaborateSdef elabState (F.Sdef ident fieldlist) =
  let
    newElabState = insertType (StructDeclName ident) (F.CStruct ident) elabState
    newFieldList = lookupFieldListType newElabState fieldlist
    fieldNameList = map (\(F.Field ctype ident) -> ident) fieldlist
    fieldNameDuplicated = (Set.size (Set.fromList fieldNameList)) < (length fieldNameList)
  in
    case fieldNameDuplicated of
      False -> (newElabState, (E.Sdef ident newFieldList))
      True -> error "Field name duplicated"

elaborateSdecl :: ElabState -> F.Gdecl -> (ElabState, E.Gdecl)
elaborateSdecl elabState (F.Sdecl ident) =
  let
    newElabState = insertType (StructDeclName ident) (F.CStruct ident) elabState
  in
    (newElabState, E.Sdecl ident)

insertFdeclMap :: F.Ident -> F.CType -> [F.Param] -> ElabState -> ElabState
insertFdeclMap ident ctype paramlist (typemap, declmap) =
  let
    paramTypeSignature = makeFunctionTypeSignature ctype paramlist
    newdeclmap = Map.insert ident paramTypeSignature declmap
  in
    (typemap, newdeclmap)

insertType :: TypeNameType -> F.CType -> ElabState -> ElabState
insertType ident newCtype elabState@(typeMap, fdeclMap) =
    (Map.insert ident newCtype typeMap, fdeclMap)

makeFunctionTypeSignature :: F.CType -> [F.Param] -> (F.CType, [F.CType])
makeFunctionTypeSignature ctype paramlist = (ctype, map getParamType paramlist)

getParamType :: F.Param -> F.CType
getParamType (F.Param ctype ident) = ctype

lookupParamType :: ElabState -> [F.Param] -> [F.Param]
lookupParamType elabState paramlist = map (lookupParam elabState) paramlist

lookupParam :: ElabState -> F.Param -> F.Param
lookupParam elabState (F.Param ctype ident) = F.Param (lookupType elabState ctype) ident


-- Converts types to basic types
lookupType :: ElabState -> F.CType -> F.CType
lookupType elabState ctype =
  case ctype of
    F.CInt -> F.CInt
    F.CBool -> F.CBool
    F.CTypeIdent ident -> lookupTypeIdent elabState (TypeDefName ident)
    F.CVoid -> F.CVoid
    F.CPtr ctype -> F.CPtr (lookupType elabState ctype)
    F.CArray ctype -> F.CArray (lookupType elabState ctype)
    F.CStruct ident -> F.CStruct ident


isTypeIdentInMap :: ElabState -> TypeNameType -> Bool
isTypeIdentInMap (typeMap, fdeclmap) ident =
  Map.member ident typeMap

lookupTypeIdent :: ElabState -> TypeNameType -> F.CType
lookupTypeIdent (typeMap, fdeclmap) ident =
  case Map.member ident typeMap of
    True -> (case typeMap Map.! ident of
          -- By our induction hypothesis, typeMap[ident] must already be a basic type
          F.CTypeIdent _ -> error("Recursive typedef not allowed:" ++ show ident)
          ctype -> ctype
        )
    False -> error("lookupIdent: ident not found:" ++ show ident ++ " in typeMap:" ++ show typeMap)

fastTypeToEastType :: F.CType -> E.CType
fastTypeToEastType F.CInt = E.CInt
fastTypeToEastType F.CBool = E.CBool
fastTypeToEastType (F.CTypeIdent ident) = error("typeident in EAST not allowed")
fastTypeToEastType F.CVoid = E.CVoid
fastTypeToEastType (F.CPtr ctype) = E.CPtr (fastTypeToEastType ctype)
fastTypeToEastType (F.CArray ctype) = E.CArray (fastTypeToEastType ctype)
fastTypeToEastType (F.CStruct ident) = E.CStruct ident

fastParamToEastParamList :: [F.Param] -> [E.Param]
fastParamToEastParamList paramlist = map fastParamToEastParam paramlist

fastParamToEastParam :: F.Param -> E.Param
fastParamToEastParam (F.Param ctype ident) = E.Param (fastTypeToEastType ctype) ident

-- Converts types in struct fields to basic types
lookupFieldListType :: ElabState -> F.Fieldlist -> E.Fieldlist
lookupFieldListType elabState fieldlist =
  map (lookupFieldType elabState) fieldlist

lookupFieldType :: ElabState -> F.Field -> E.Field
lookupFieldType elabState (F.Field ctype ident) =
  (E.Field (fastTypeToEastType $ lookupType elabState ctype) ident)

-- convertSdeclToSdefState :: ElabState -> ElabState
-- convertSdeclToSdefState (typemap, declmap) =
--   let
--     newTypeMap = Map.mapKeys convertSdeclToSdef typemap
--   in
--     (newTypeMap, declmap)
-- 
-- convertSdeclToSdef :: TypeNameType -> TypeNameType
-- convertSdeclToSdef (StructDeclName ident) = StructDeclName ident
-- convertSdeclToSdef other = other

