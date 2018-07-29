module Compile.LivenessAnalysis where

import Compile.Types
import qualified Data.Set as Set
import qualified Data.Map as Map
import Compile.Dataflow
import Compile.RegisterList
import qualified Data.Vector.Mutable as M
import Control.Monad.ST
import Control.Monad

import qualified Data.Vector as V


findLiveSets :: [IAsm] -> V.Vector (IAsm, [Int], [Int]) -> [(Set.Set ILoc, Set.Set Rule, IAsm)]
findLiveSets prog progFlow =
  let
    (vectProg, vectSucc, vectPred) = V.unzip3 progFlow
    vectUseDef = V.map findUsedAndDefined vectProg
    len = V.length vectProg
--    startingLivesets = Map.fromAscList (map (\i -> (i, Set.empty)) [0 .. (len - 1)])
    startingLivesets = V.fromList (map (\i -> Set.empty) [0 .. (len - 1)])
    vectLiveSets = livenessMonadDriver startingLivesets vectPred vectUseDef
{-    mapLiveSets = V.foldr --foldr does in REVERSE
                    (variableTraversal vectPred vectUseDef)
                    startingLivesets
                    (V.generate len (\i -> i)) -}
  in
    V.toList (V.generate len (\i -> (
        vectLiveSets V.! i,
        vectUseDef V.! i,
        vectProg V.! i
      )
      ))



livenessMonadDriver :: V.Vector (Set.Set ILoc) -> V.Vector [Int] -> V.Vector (Set.Set Rule) -> V.Vector (Set.Set ILoc)
livenessMonadDriver startingLiveness predecessors vectUseDef =
  runST $ do
    workingVect <- V.thaw startingLiveness
    forM_ (reverse [0 .. (V.length predecessors) - 1]) (seedLine predecessors vectUseDef workingVect)
    V.freeze workingVect

-- seedLine a b v l = return v

-- seedLine :: V.Vector [Int] -> V.Vector (Set.Set Rule) -> M.MVector ST (Set.Set ILoc) -> Int -> M.MVector ST (Set.Set ILoc)
seedLine preds vectUseDef workingVect linenum = do
  knownLive <- M.read workingVect linenum
  forM_ (Set.toList (Set.difference lineUse knownLive)) (propagate preds vectUseDef workingVect linenum ) 
  where
    lineUse :: Set.Set ILoc
    lineUse = Set.map getLoc $ Set.filter (not . isDef) $ vectUseDef V.! linenum

propagate preds vectUseDef workingVect linenum var =
  do
    knownLive <- M.read workingVect linenum
    if (Set.member var knownLive) || (Set.member (Def var) (vectUseDef V.! linenum)) then return () 
      else 
        do
          M.write workingVect linenum (Set.insert var knownLive) 
          forM_ (preds V.! linenum) (\line -> propagate preds vectUseDef workingVect line var) 



propagateVariableAlongCFG :: V.Vector [Int] -> V.Vector (Set.Set Rule) -> ILoc -> Bool -> Int -> Set.Set Int -> Set.Set Int
-- propagateVariableAlongCFG vectPred vectUseDef localvar isLiveInSucc lineNum setLiveLines
-- Args:
--   vectPred: vectPred[i] is a list of the predecessor line numbers for line i, so it represents dataflow for the entire program.
--   vectUseDef: vectUseDef[i] is a set of the used and defined variables for line i
--   localvar: The local variable that we are doing liveness analysis on, according to the algorithm below
--   isLiveInSucc: Is localvar live in the successor of line linenum?
--   lineNum: The line number we are currently at.
--   setLiveLines: The set of lines where localvar is live at. In set notation: {x: local var is live at line x}
-- Outputs:
--   (new)setLiveLines: The set of lines where localvar is live at after propagating backwards through the CFG.
--                      In set notation: {x: local var is live at line x}
-- Description: Propagates a local variable backwards along the Control Flow Graph.
-- We walk backwards along the control flow edges propagating only the information for
-- the single variable, stopping if it is already known to be live when we reach a line,
-- or if it is defined and not used at that line. (Lecture 4, Page 9)

propagateVariableAlongCFG vectPred vectUseDef localvar isLiveInSucc lineNum setLiveLines =
  let
    isLiveAlready = Set.member lineNum setLiveLines
    setUseDef = vectUseDef V.! lineNum
    isDefined = Set.member (Def localvar) setUseDef
    isUsed = Set.member (Use localvar) setUseDef
  in
  -- stop if it is already known to be live when we reach a line, or if it is defined and not used at that line (Lecture 4, Page 9)
  case (isLiveAlready, isDefined, isUsed) of
    (True, _, _) -> setLiveLines --already live
    (_, True, False) -> setLiveLines -- defined and not used
    (_, _, _) -> (-- Else, propagate
      let
        setNewLiveLines = (
          case (isUsed, isDefined, isLiveInSucc) of
            (True, _, _) -> Set.insert lineNum setLiveLines -- Rule 1: use(l,x) implies live(l,x) (Lect 4, Page 8)
            (_, False, True) -> Set.insert lineNum setLiveLines -- Rule 2: not(def(l,u)), succ(l,l'), live(l',u) implies live(l,u) (Lect 4 Page 8)
            (_, _, _) -> setLiveLines
          )
        isLiveInCurrentLine = Set.member lineNum setNewLiveLines
        in
          -- Propagate along ALL predecessors
          -- Recall: fold :: (a -> b -> b) -> b -> Set a -> b
          foldr 
            (propagateVariableAlongCFG vectPred vectUseDef localvar isLiveInCurrentLine)
            setNewLiveLines
            (vectPred V.! lineNum)
      )

-- Same as above, but we initialize setLiveLines to Set.empty first, and isLiveInSucc to false.
propagateVariableAlongCFGWrapper :: V.Vector [Int] -> V.Vector (Set.Set Rule) -> Int -> ILoc -> Map.Map Int (Set.Set ILoc) -> Map.Map Int (Set.Set ILoc)
propagateVariableAlongCFGWrapper vectPred vectUseDef lineNum localvar mapLiveSet =
  let 
    setLiveLines = propagateVariableAlongCFG vectPred vectUseDef localvar False lineNum Set.empty
    listLiveLines = Set.toList setLiveLines
    listUpdate = map (\lineNum -> (lineNum, Set.insert localvar (mapLiveSet Map.! lineNum))) listLiveLines
  in
    foldl (\m (l, update) -> Map.insert l update m) mapLiveSet listUpdate

-- Is the rule a use rule?
isUse :: Rule -> Bool
isUse (Use _) = True
isUse (Def _) = False

variableTraversal :: V.Vector [Int] -> V.Vector (Set.Set Rule) -> Int -> Map.Map Int (Set.Set ILoc) -> Map.Map Int (Set.Set ILoc)
-- variableTraversal vectPred vectUseDef lineNum setLiveLines 
-- When we arrive at
-- a line and see which variables are used there. For each such variable, we see if it is
-- already live, in which case we do nothing. If it is not live, we declare it so and then
-- walk backwards along the control flow edges propagating
variableTraversal vectPred vectUseDef lineNum mapLiveSet =
  let
    setUsedVariables = Set.map getLoc (Set.filter isUse (vectUseDef V.! lineNum))
    setLiveVariables = mapLiveSet Map.! lineNum
    setUsedAndNotLive = Set.difference setUsedVariables setLiveVariables
  in
    Set.fold
      (propagateVariableAlongCFGWrapper vectPred vectUseDef lineNum)
      mapLiveSet
      setUsedAndNotLive

-- Takes a list of rules for the program (reversed, so the last line's used and defined set
-- is at the top of the list) and the previous (really next) line's live variables and gives the
-- live sets for the rest of the program.
findLiveSetsHelp :: Set.Set ILoc -> [Set.Set Rule] -> [Set.Set ILoc]
findLiveSetsHelp _ [] = []
findLiveSetsHelp s (x:xs) =
  let
    defined = Set.map getLoc $ Set.filter isDef x
    used = Set.map getLoc $ Set.filter (not . isDef) x
    thisline = Set.union used $ Set.difference s defined
  in
    thisline:(findLiveSetsHelp thisline xs)

colorOutArg :: Int -> ILoc
colorOutArg n =
  case n of
    0 -> IReg 5 --edi
    1 -> IReg 4 --esi
    2 -> IReg 3 --edx
    3 -> IReg 2 --ecx
    4 -> IReg 7 --r8
    5 -> IReg 8 --r9
    m -> IArg (m)

-- Determines used and defined rules for a given line
findUsedAndDefined :: IAsm -> Set.Set Rule
findUsedAndDefined (IAsm [dest] ANop arg) =
  case arg of
    ILoc loc -> Set.fromList [Def dest, Use loc]
    _ -> Set.singleton (Def dest)
findUsedAndDefined (IAsm [dest] op arg) =
  let
    newSet = specialBinOps op
  in
    case arg of
      IImm _ -> Set.union (Set.fromList [Def dest, Use dest]) newSet
      ILoc loc -> Set.union (Set.fromList [Def dest, Use loc, Use dest]) newSet
findUsedAndDefined (IRet (ILoc loc)) = Set.fromList [Use loc]
findUsedAndDefined (IRet (IImm _)) = Set.singleton (Def (IReg 0))
findUsedAndDefined (ICall _ n) = 
  Set.union (Set.fromList (map (\i -> Use (colorOutArg (i-1))) [1.. n]))
  (Set.fromList [Def (IReg 0)])
findUsedAndDefined _ = Set.empty

-- Adds rules for div and mod, should be extended if we find other special binary operations
specialBinOps :: AOp -> Set.Set Rule
specialBinOps ADiv = Set.fromList [Def (IReg 0), Def (IReg 3), Use (IReg 0), Use (IReg 3)]-- Uses and defines eax, edx
specialBinOps AMod = Set.fromList [Def (IReg 0), Def (IReg 3), Use (IReg 0), Use (IReg 3)]-- Uses and defines eax, edx
specialBinOps AShiftL = Set.fromList [Use (IReg 2)] -- Uses ecx
specialBinOps AShiftR = Set.fromList [Use (IReg 2)] -- Uses ecx
specialBinOps _  = Set.empty

-- Is a rule a definition rule?
isDef (Def _) = True
isDef _ = False

-- Gets the location from a rule
getLoc (Def loc) = loc
getLoc (Use loc) = loc