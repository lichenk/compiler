module Compile.ParameterizeJAsm where

import Compile.Types
import Compile.Types.JumpAbstractAssembly
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Compile.MapWrap as MapWrap

import qualified Data.Vector as V

third3 :: (a, b, c) -> c
third3 (v1, v2, v3) = v3

parameterize :: [(Set.Set JLoc, Set.Set JRule, JAsm)] -> [JAsm]
parameterize l = 
  let
    labelMapping = collectLabels (Map.empty) l
    updateLine line =
      case line of
        JJmp target _ t -> JJmp target (map (\loc -> JLoc loc) (labelMapping Map.! target)) t
        JLabel label _ -> JLabel label (labelMapping Map.! label)
        _ -> line
  in
    map (updateLine . third3) l

collectLabels :: Map.Map Int [JLoc] -> [(Set.Set JLoc, Set.Set JRule, JAsm)] -> Map.Map Int [JLoc]
collectLabels m [] = m
collectLabels m (line:xs) = 
  case line of
    (live, _, JLabel labelNum _) -> collectLabels (Map.insert labelNum (Set.toAscList live) m) xs
    _ -> collectLabels m xs