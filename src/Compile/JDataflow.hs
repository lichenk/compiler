module Compile.JDataflow where

import qualified Compile.MapWrap as MapWrap
import Compile.Types
import Compile.Types.JumpAbstractAssembly
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Vector as V

buildLabelMapping :: V.Vector JAsm -> Map.Map Int Int
buildLabelMapping prog = 
  let
    newmap mapping linenum line =
      case line of 
        JLabel n _ -> Map.insert n linenum mapping
        _ -> mapping
  in
    V.ifoldl newmap (Map.empty) prog

findSuccessors :: Map.Map Int Int -> Int -> Int -> JAsm -> [Int]
findSuccessors m _ _ (JJmp n _ RJump) = [m Map.! n]
findSuccessors m _ linenum (JJmp n _ _) = [m Map.! n, linenum + 1]
findSuccessors _ _ _ (JRet _) = []
findSuccessors _ lengthprog linenum _ = 
  -- Last line may not be a return instruction! (It can be a label)
  -- So we must force it to have no successors in this case
  case ((linenum+1) < lengthprog) of
    True -> [linenum + 1]
    False -> []

-- Gives a vector of (instruction, successors, predecessors)
findControlFlow :: [JAsm] -> V.Vector (JAsm, [Int], [Int])
findControlFlow l =
  let
    prog = V.fromList l
    labelmap = buildLabelMapping prog
    successors = V.imap (findSuccessors labelmap (V.length prog)) prog
    predecessors = Map.fromListWith (++) $ buildPredecessorList successors
  in
    V.imap (\i l -> (l, successors V.! i, Map.findWithDefault [] i predecessors)) prog

-- Takes the successor vector and makes the predecessor list [(line, [predecessor])]
-- Note that each predecessor is actually a single predecessor, they're in list form
-- for being made into a map
buildPredecessorList :: V.Vector [Int] -> [(Int, [Int])]
buildPredecessorList successors =
  let
    -- i is the line, l is the lines it goes to
    zipped i l = map (\loc -> (loc, [i])) l
    combine list linenum line =
      (zipped linenum line) ++ list
  in
    V.ifoldl combine [] successors
