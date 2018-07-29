module Compile.ProcessLabels where

import Compile.Types.JumpAbstractAssembly
import Compile.Types
import qualified Data.Map as Map
import qualified Compile.MapWrap as MapWrap
import Compile.RegisterList
import Data.Tuple
import Debug.Trace
import Data.List.Split

-- preprocessJAsm removes consecutive labels, and puts explicit gotos before labels
preprocessJAsm :: [JAsm] -> [JAsm]
preprocessJAsm prog = addGotos $ removeRedundantLabels prog

-- addGotos makes it so that any label which can be reached by an immediate neighbor gets a goto before it.
addGotos :: [JAsm] -> [JAsm]
addGotos [] = []
addGotos [instruction] = [instruction]
addGotos (inst1:inst2:rest) =
  case [inst1, inst2] of
    [JJmp _ _ RJump, JLabel _ _] -> inst1 : (addGotos (inst2:rest))
    [_, JLabel l vars] -> inst1 : (JJmp l [] RJump) : (addGotos (inst2:rest))
    [_, _] -> inst1 : (addGotos (inst2:rest))

-- removeRedundantLabels identifies chains of labels and chooses one label for all of them. 
removeRedundantLabels :: [JAsm] -> [JAsm]
removeRedundantLabels prog = 
  let
    labelMap = compressMapping $ scanLabels prog Map.empty
  in
    concatMap (updateLabel labelMap) prog

scanLabels :: [JAsm] -> Map.Map Int Int -> Map.Map Int Int
scanLabels [] m = m
scanLabels ((JLabel n1 vars1):(JLabel n2 vars2):rest) m =
  scanLabels ((JLabel n2 vars2):rest) (Map.insert n1 n2 m)
scanLabels ((JLabel n v):rest) m = scanLabels rest (Map.insert n n m)
scanLabels (x:xs) m = scanLabels xs m

updateLabel :: Map.Map Int Int -> (JAsm ->[JAsm])
updateLabel m =
  let
    f line =
      case line of
        (JLabel n _) ->
          if (n == (m Map.! n))
          then [line]
          else []
        (JJmp n vars jtype) -> [JJmp (m Map.! n) vars jtype]
        _ -> [line]
  in 
    f

compressMapping :: Map.Map Int Int -> Map.Map Int Int
compressMapping m = foldl compressValue m $ Map.keys m

compressValue :: Map.Map Int Int -> Int -> Map.Map Int Int
compressValue m n =
  if (m Map.! n) == n 
  then m
  else
    let
      newMap = compressValue m (m Map.! n)
    in
      Map.insert n (newMap Map.! (m Map.! n)) newMap
