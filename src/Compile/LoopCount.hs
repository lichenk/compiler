{- We're giving each variable a score, based on how many loops it's in.
  The scoring algorithm is going to look like this:
  A loop nested n levels deep will have a score of n
  The score of a variable is the maximum score of every loop
  which contains it. -}
module Compile.LoopCount where

import Compile.Types.IntermediaryAssembly
import Compile.Types.Ops
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace

determineLoopcount :: [IAsm] -> [(IAsm, Int)]
determineLoopcount prog =
  let
    getLabel line =
      case line of
        IAsmLabel str -> Set.singleton str
        _ -> Set.empty
    getTarget line =
      case line of
        IAsmJump _ str -> Set.singleton str
        _ -> Set.empty
    labels = map getLabel prog
    targets = map getTarget prog
  in 
    zip prog $ map (Set.size) $ zipWith Set.intersection (scanl1 Set.union labels) (scanr1 Set.union targets)

scoreVariables :: [IAsm] -> Map.Map ILoc Int
scoreVariables prog =
  let
    loopcounts = determineLoopcount prog
  in
    gatherCounts loopcounts Map.empty 

gatherCounts :: [(IAsm, Int)] -> Map.Map ILoc Int -> Map.Map ILoc Int
gatherCounts [] m = m
gatherCounts ((line, depth):rest) m = 
  case line of
    IAsm dests _ srcs ->
      let
        locs = dests ++ (getLoc srcs)
        updateMap mapping loc = Map.insertWith max loc depth mapping
        newMap = (foldl updateMap) m locs
      in
        gatherCounts rest newMap
    _ -> gatherCounts rest m

getLoc :: IVal ->[ILoc]
getLoc x =
  case x of
    ILoc loc -> [loc]
    _ -> []