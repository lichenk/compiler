module Compile.LLVMStructGen(buildLLVMStructs, convertType, genLLVMStruct, buildArrayStructs, mergeLookup,makeArrayStructNameL, LGenStructInfo) where
import Compile.Types.TAST
import Compile.Types.LLVM
import Compile.Constants
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Tuple
import Compile.Types
import Debug.Trace
import qualified Compile.MapWrap as MapWrap
-- WARNING: Padding is not supported!
-- Former padding code used the size of a struct as its alignment. But that is incorrect.
-- The alignment of a struct should be the maximum alignment of its fields
-- NOT the size of the struct!!

-- CType -> Int: Sends struct to size
-- LType -> String -> (Int,LType): Sends struct type and field name to (index, field type)
type LGenStructInfo = (CType -> Int, LType -> String -> (Int,LType))

-- defineAllStructs structs = map defineStruct structs
-- 
-- -- %struct.foo = type { i32, %struct.bar }
-- defineStruct (Sdef ident fieldlist) =
--   let
--     fields = List.intercalate ", " (map showField fieldlist)
--   in
--     LSdecl ident [(LType, String)]
--     LStr ("%struct." ++ ident ++ " = type { " ++ fields ++ " }")
-- 
-- showField (Field ctype ident) =
--   (show $ convertType ctype)

mergeLookup :: LGenStructInfo -> Map.Map LType (Map.Map String (Int,LType)) -> LGenStructInfo
mergeLookup (whocares, oldLookup) structMap =
  let
    newLookup :: LType -> String -> (Int,LType)
    newLookup t f =
      case Map.member t structMap of
        True -> (structMap Map.! t) Map.! f
        False -> oldLookup t f
  in
    (whocares, newLookup)

buildArrayStructs :: [Gdecl] -> ([LSdecl], Map.Map LType (Map.Map String (Int,LType)))
buildArrayStructs gdecls =
  let
    typelist = (concat (List.map getTypesGdecl gdecls))++[CArray CInt, CArray CBool, CArray $ CPtr CInt, CArray $ CPtr CBool, CArray $ CArray CInt, CArray $ CArray CBool]
    typeListUnique = Set.toList $ Set.fromList typelist
    arrTypeList = concatMap findArrayType typeListUnique
    arrTypeListUnique = (Set.toList $ Set.fromList arrTypeList)
    sdeclLookupTuple = map makeArrayStruct arrTypeListUnique
    (sdeclList, lookupList) = unzip sdeclLookupTuple
    lookupMap = Map.fromList lookupList
  in
    (sdeclList, lookupMap)

-- Given a type, outputs all array types inside it
-- Does not explore into structs
-- Example input: CPtr CArray CPtr CArray CInt
-- Output: [CPtr CArray CInt, CIny]
-- Example input: CStruct foo, where foo is declared as struct foo {int[] x;}
-- Output: []. Even though foo contains an array, we don't explore it.
-- This is because arrays inside structs are already added by getTypesGdecl!
findArrayType :: CType -> [CType]
findArrayType (CArray ctype) = ctype:(findArrayType ctype)
findArrayType (CPtr ctype) = findArrayType ctype
findArrayType _ = []

makeArrayStruct :: CType -> (LSdecl, (LType, Map.Map String (Int,LType)))
makeArrayStruct (ctype) = 
  let
    structName = makeArrayStructName ctype
    ltype = convertType ctype
    look = (LPtr $ LStruct structName, Map.fromList [("len", (0,LInt32)), ("arr", (1, LPtr $ LArray ltype))])
  in
    (LSdecl structName [(LInt32, "len"),(LPtr $ LArray ltype, "arr")], look)

getTypesGdecl :: Gdecl -> [CType]
getTypesGdecl (Fdefn ctype ident paramlist block) =
  getTypes block ++ getParamTypes paramlist ++ [ctype]
getTypesGdecl (Sdef ident fieldlist) = getFieldTypes fieldlist
getTypesGdecl _ = []

getFieldTypes :: [Field] -> [CType]
getFieldTypes l = map getFieldType l
getFieldType :: Field -> CType
getFieldType (Field t _) = t

getParamTypes :: [Param] -> [CType]
getParamTypes params = map getParamType params
getParamType :: Param -> CType
getParamType (Param t _) = t

-- Get the decls from   
getTypes :: [Stmt] -> [CType]
getTypes stmts = concatMap getTypesStmt stmts

getTypesStmt :: Stmt -> [CType]
getTypesStmt (Simp (Decl t v)) = [t]
getTypesStmt (Simp _) = []
getTypesStmt (Ctrl ctrl) = getTypesCtrl ctrl
getTypesStmt (Blk block) = getTypes block

getTypesCtrl :: Ctrl -> [CType]
getTypesCtrl (If _ s1 s2) = getTypesStmt s1 ++ getTypesStmt s2
getTypesCtrl (While e s) = getTypesStmt s
getTypesCtrl (Ret _) = []
getTypesCtrl (RetVoid) = []



buildLLVMStructs :: [Gdecl] -> LGenStructInfo
buildLLVMStructs structs =
  let
    sizelookup = buildStructs structs
    fieldmap = foldl genFieldMap Map.empty structs
    fieldlookup t f =
      case t of
        LStruct structname -> trace ("Warning: should not be looking up raw struct instead of struct*" ++ show t) $ (fieldmap Map.! structname) Map.! f
        LPtr (LStruct structname) -> (fieldmap Map.! structname) Map.! f
        _ -> error ("Invalid type for lookup:" ++ show t ++ " for field " ++ show f)
  in
    (sizelookup, fieldlookup)


-- Makes a map:
-- map[struct name][field name] = field index, field type
genFieldMap :: Map.Map String (Map.Map String (Int, LType)) -> Gdecl -> Map.Map String (Map.Map String (Int, LType))
genFieldMap m (Sdef name fields) =
  let
    fieldIndexZipped = zip fields [0..]
    fieldmap = foldl insertFieldToFieldMap Map.empty fieldIndexZipped
  in
    Map.insert name fieldmap m

-- Makes a map:
-- map[field name] = field index, field type
insertFieldToFieldMap :: Map.Map String (Int, LType) -> (Field,Int) -> Map.Map String (Int, LType)
insertFieldToFieldMap m ((Field t fieldname), idx) =
  Map.insert fieldname (idx, (convertType t)) m


buildStructs structs = 
  let
    structmap = foldl genStruct Map.empty structs
    sizestructsmap = Map.map fst structmap
    fieldmap = Map.map snd structmap
    sizelookup t =
      case t of
        CInt -> 4
        CBool -> 4
        CVoid -> 4
        CPtr _ -> 8
        CArray _ -> 8
        CStruct i -> fst $ structmap Map.! i
        CAny -> 8
        CRawArray _ -> 32 --strictly speaking, 12 is enough
        other -> error ("Ident or NoType in struct generation:" ++ show other)
  in
    sizelookup

findSize :: Map.Map String (Int, Map.Map String Int) -> CType -> Int
findSize m t =
  case t of
    CInt -> 4
    CBool -> 4
    CArray _ -> 8
    CStruct s -> fst $ m Map.! s
    CPtr _  -> 8

addField :: (CType -> Int) -> (Map.Map String Int, Int) -> (CType, String) -> (Map.Map String Int, Int)
addField f (m, offset) (t, name) =
  let
    elemsize = f t
    -- padding = if offset `mod` elemsize == 0 then 0 else elemsize - (offset `mod` elemsize)
    padding = 0
    newlocation = offset + padding
  in
    (Map.insert name newlocation m, newlocation + elemsize)

genStruct :: Map.Map String (Int, Map.Map String Int) -> AST -> Map.Map String (Int, Map.Map String Int)
genStruct current_structs (Sdef name fields) =
  let
    fieldAdder :: (Map.Map String Int, Int) -> (CType, String) -> (Map.Map String Int, Int)
    fieldAdder = addField (findSize current_structs)
    maxalign = foldl max 1 $ map (findSize current_structs) $ map getCType fields
    rawFields = map (\(Field t n) -> (t, n)) fields
    (mymap, final_offset) = foldl fieldAdder (Map.empty, 0) rawFields
    -- structpadding = if final_offset `mod` maxalign == 0 then 0 else maxalign - (final_offset `mod` maxalign)
    structpadding = 0
    mysize = final_offset + structpadding
  in
    Map.insert name (mysize, mymap) current_structs

getCType :: Field -> CType
getCType (Field t _) = t



-- Will match-fail on anything except sdecls
genLLVMStruct :: Gdecl -> LSdecl
genLLVMStruct (Sdef name fields) =
  LSdecl name $ map (\(Field t n) -> (convertType t, n)) fields

makeArrayStructNameL ltype = arrayStructPrefix ++ (serializeType ltype)
makeArrayStructName ctype = arrayStructPrefix ++ (serializeType $ convertType ctype)

convertType :: CType -> LType
convertType t =
  case t of
    CInt8 -> LInt8 -- Used for voids
    CInt -> LInt32
    CBool -> LBool
    CTypeIdent i -> error "We should have gotten rid of idents already"
    CVoid -> LVoid
    CPtr innerType -> LPtr $ convertType innerType
    CArray innerType -> LPtr $ LStruct (makeArrayStructName innerType)
    CRawArray innerType -> LStruct (makeArrayStructName innerType)
    CStruct i -> LStruct i
    CAny -> error "Why is there a CAny here?"
    CEightByte -> LInt64
    CNoType -> error "Why is there a CNoType here?"

