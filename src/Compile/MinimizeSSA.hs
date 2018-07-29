module Compile.MinimizeSSA where
import qualified Data.Map as Map
import Compile.JDataflow
import Compile.Types
import Compile.Types.JumpAbstractAssembly
import Data.List.Split
import qualified Data.Vector as V



minimize :: [JAsm] -> [JAsm]
minimize prog =
  let
    labelmap = makeLabelMapping prog
    blockList = splitBasicBlocks prog
    blockMap = Map.fromAscList $ zip [0 ..] (map (\block -> Map.fromAscList (zip [0 ..] block)))
    lineToBlock = V.fromList $ basicBlockLineNumMapping blockList


minimizeHelp :: Map.Map Int (Map.Map Int JAsm) -> V.Vector (Int, Int) -> (Map.Map Int (Map.Map Int JAsm), Bool)
minimizeHelp progMap lineToPos currentBlock =
  let
    



splitBasicBlocks :: [JAsm] -> [[JAsm]]
splitBasicBlocks prog = split (keepDelimsL $ whenElt isLabel) prog

-- Takes a program and returns a map from label number to the line numbers which jump to that label
makeLabelMapping :: [JAsm] -> Map.Map Int [Int]
makeLabelMapping prog =
  let
    dataflow = findControlFlow prog
    isTupleLabel (line, _, _) = isLabel line
    onlyLabels = V.filter isTupleLabel dataflow
    getLabelToPreds (JLabel label _, _, preds) = (label, preds)
  in
    Map.fromList $ V.toList (V.map getLabelToPreds onlyLabels)

basicBlockLineNumMapping :: [[JAsm]] -> [(Int, Int)]
basicBlockLineNumMapping blocks =
  let
    blockSizes = map length blocks
    firstInstrList = scanl (+) 0 blockSizes
    zipped = zip3 [0 .. (length blocks)-1] firstInstrList blockSizes -- (blocknum, startingLine, codeSize)
  in
    concatMap (\(bnum, startLine, numLines) -> map (\i -> (bnum, startLine+i)) [0 .. numLines-1]) zipped


basicBlockify :: [JAsm]

isLabel line = 
  case line of
    (JLabel _ _) -> True
    _ -> False