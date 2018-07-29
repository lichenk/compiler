module Compile.GenerateStruct(buildStructs,GenStructInfo) where
import qualified Data.Map as Map
import Data.Tuple
import Compile.Types
import qualified Compile.MapWrap as MapWrap

type GenStructInfo = (CType -> Int, CType -> String -> Int)

buildStructs :: [AST] -> (CType -> Int, CType -> String -> Int)
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
        _ -> error "Ident or NoType in struct generation."
    fieldlookup t f =
      case t of
        CStruct i -> (snd $ structmap Map.! i) Map.! f
        _ -> error "Invalid type for lookup"
  in
    (sizelookup, fieldlookup)

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