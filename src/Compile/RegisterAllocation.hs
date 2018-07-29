module Compile.RegisterAllocation where

import Data.Tuple
import Compile.LivenessAnalysis
import Compile.Types
import Data.List
import Data.Maybe
import Debug.Trace
import Compile.Dataflow
import qualified Compile.MapWrap as MapWrap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V 
type InterferenceGraph = Map.Map ILoc (Set.Set ILoc)
type Coloring = Map.Map ILoc Int

-- Takes an entire Intermediate Assembly program and returns a mapping
-- from location to integer, where the integer is the number of the register
-- that variable should map to.
allocateRegisters :: V.Vector (IAsm, [Int], [Int]) -> [(Set.Set ILoc, Set.Set Rule, IAsm)] -> [IAsm] -> Map.Map ILoc Int -> (Coloring)
allocateRegisters progFlow analyzedProgram program loopMapping =
  let
    allLocs = getAllLocs program Set.empty
    graph = findInterferenceGraph progFlow analyzedProgram program
    initialColoring = Map.fromList (map (\x -> (IReg x CNoType, x)) [0..14])
    vertexOrdering = orderNodes graph loopMapping (filter (\x -> not(isReg x)) (Map.keys graph))
    coloring = colorGraph graph vertexOrdering initialColoring
  in
    coloring

-- Stupid coloring algorithm but it works
colorGraph :: InterferenceGraph -> [ILoc] -> Coloring -> Coloring
colorGraph graph (v:vertices) coloring =
  let
    neighbors = case Map.lookup v graph of
      Just x -> x
      Nothing -> error("Bug in RegisterAllocation: Vertex not found in graph.")
    coloredNeighbors = filter (\x -> Map.member x coloring) (Set.toList neighbors)
    neighborColors = map (\x -> coloring Map.! x) coloredNeighbors
    minColor = minout neighborColors
    newColoring = Map.insert v minColor coloring
  in
    colorGraph graph vertices newColoring
colorGraph graph [] coloring = coloring

-- Takes the original Intermediate Assembly program and returns an Interference Graph
-- The procedure we do is this:
-- 1. Get the Control Flow Graph and Live sets
-- 2. For each line n,
--    Let D_n (set of ILocs) be the set of variables defined at line n.
--    Let successors S_n (set of line numbers) be the successors of line n.
-- 3. We create an edge (l,d) for all d in D_n and live variables l in the successors S_n.
findInterferenceGraph :: V.Vector (IAsm, [Int], [Int]) -> [(Set.Set ILoc, Set.Set Rule, IAsm)] -> [IAsm] -> InterferenceGraph
findInterferenceGraph progFlow analyzedProgram program =
  let
    analyzedProgramVect = V.fromList analyzedProgram
    -- Initialize graph to contain all precolored registers
    initialGraph = Map.fromList (map (\x -> (IReg x CNoType, Set.empty)) [0..14])
    -- Find CFG and Live sets
    (vectProg, vectSucc, vectPred) = V.unzip3 progFlow
  in
    -- (addInterference vectSucc analyzedProgram) is of type
    -- InterferenceGraph (Old graph) -> Int (LineNumber) -> InterferenceGraph
    -- Where each step of foldr adds all edges between defined variables at line n
    -- and live variables in the successors of line n.
    -- Should we foldr in reverse instead?
    foldl (addInterference vectSucc analyzedProgramVect) initialGraph [0..(length analyzedProgram -1)]

-- (addInterference vectSucc analyzedProgram) is of type
-- InterferenceGraph (Old graph) -> Int (LineNumber) -> InterferenceGraph (New graph including
-- all edges between defined variables at line n, and live variables in the successors of line n)
-- Let D_n (set of ILocs) be the set of variables defined at line n.
-- Let successors S_n (set of line numbers) be the successors of line n.
-- We insert an edge (l,d) for all d in D_n and live variables l in the successors S_n.
-- Recall: analyzedProgram is of type [(Set.Set ILoc, Set.Set Rule, IAsm)]
addInterference:: V.Vector [Int] -> V.Vector (Set.Set ILoc, Set.Set Rule, IAsm) -> InterferenceGraph -> Int -> InterferenceGraph
addInterference vectSucc analyzedProgram graph linenum =
  let
    currentRules = getRules analyzedProgram linenum
    currentDefinedVariables = Set.map getDef (Set.filter isDef currentRules)
    successors = vectSucc V.! linenum
    successorLiveSetList = getLiveSetList analyzedProgram successors
    successorLiveSetUnioned = foldl Set.union Set.empty successorLiveSetList
  in
    addCompleteBipartiteSubgraph currentDefinedVariables successorLiveSetUnioned graph

-- addCompleteSubgraph A B graph
-- Given sets A,B, we insert the complete bipartite graph (A,B) into the graph
addCompleteBipartiteSubgraph :: Set.Set ILoc -> Set.Set ILoc -> InterferenceGraph -> InterferenceGraph
addCompleteBipartiteSubgraph set1 set2 graph =
  let
    -- Recall: fold :: (a -> b -> b) -> b -> Set a -> b
    graphSet1ToSet2 = Set.fold (addSetToVertex set2) graph set1
    finalGraph = Set.fold (addSetToVertex set1) graphSet1ToSet2 set2
  in
    finalGraph

-- addSetToVertex
-- Given a set L and vertex v, insert edges (l,v) for all l in L
addSetToVertex:: Set.Set ILoc -> ILoc -> InterferenceGraph -> InterferenceGraph
addSetToVertex liveset vertex graph =
  case (Map.lookup vertex graph) of
    -- If neighbors of vertex exist, union in the liveset
    Just neighbors -> Map.insert vertex (Set.union neighbors liveset) graph
    -- Else, set the liveset to be the neighbors of vertex
    Nothing -> Map.insert vertex liveset graph

getLiveSetList:: V.Vector (Set.Set ILoc, Set.Set Rule, IAsm) -> [Int] -> [Set.Set ILoc]
getLiveSetList analyzedProgram successors =
  let
    liveVector = analyzedProgram
  in
    map (getLiveSet liveVector) successors

getLiveSet:: V.Vector (Set.Set ILoc, Set.Set Rule, IAsm) -> Int -> Set.Set ILoc
getLiveSet analyzedProgram linenum =
  let
    (liveset, _, _) = case (linenum < (V.length analyzedProgram)) of
      True -> analyzedProgram V.! linenum
      False -> error("getLiveSet: linenum out of range" ++ show linenum)
  in
    liveset

getRules:: V.Vector (Set.Set ILoc, Set.Set Rule, IAsm) -> Int -> Set.Set Rule
getRules analyzedProgram linenum =
  let
    (_, rules, _) = case (linenum < (V.length analyzedProgram)) of
      True -> analyzedProgram V.! linenum
      False -> error("getRules: linenum out of range" ++ show linenum)
  in
    rules


-- minout - Somewhat Optimized, O(n log n)
minout :: [Int] -> Int
minout l = minoutHelp (sort l) 0

minoutHelp :: [Int] -> Int -> Int
minoutHelp [] b = b
minoutHelp (x:xs) b = if x > b then b else if x < b then minoutHelp xs b else minoutHelp xs (b + 1)

getAllLocs :: [IAsm] -> Set.Set ILoc -> Set.Set ILoc
getAllLocs [] s = s
getAllLocs (x:xs) s =
  case x of 
    IAsm [dest] op (ILoc loc) -> getAllLocs xs (Set.union s (Set.fromList [dest, loc])) 
    IAsm [dest] op _ -> getAllLocs xs (Set.insert dest s) 
    IRet (ILoc loc) -> getAllLocs xs (Set.insert loc s) 
    _ -> getAllLocs xs s 

isReg :: ILoc -> Bool
isReg (IReg _ _) = True
isReg _ = False

isDef :: Rule -> Bool
isDef (Def _) = True
isDef _ = False

-- Gets the location from a rule
getDef :: Rule -> ILoc
getDef (Def loc) = loc
getDef (Use loc) = error("getDef: not a Def")


orderNodes :: InterferenceGraph -> Map.Map ILoc Int -> [ILoc] -> [ILoc]
orderNodes graph loopMap nodes =
  let
    scoredNodes = map (orderingHeuristic graph loopMap) nodes
    cmp = (\(_, score1) (_, score2) -> compare score1 score2)
  in
    map fst $ reverse $ sortBy cmp scoredNodes

orderingHeuristic :: InterferenceGraph -> Map.Map ILoc Int -> ILoc -> (ILoc, Int)
orderingHeuristic graph loopMap node =
  let
    deg = Set.size $ graph Map.! node
    loopNum = Map.findWithDefault 0 node loopMap
  in
    (node, deg + (50 * loopNum))

-- Used with the loop counter
ilocToAloc :: ILoc -> ALoc
ilocToAloc (IReg n _) = (AReg n CNoType)
ilocToAloc (ITemp n _) = (ATemp n CNoType)
ilocToAloc (IMem n _) = (AMem n CNoType)
ilocToAloc (IInArg n _) = (AInArg n CNoType)
ilocToAloc (IArg n _) = AArg n CNoType
ilocToAloc (IHeap val _) = AHeap (ivalToAval val) CNoType

ivalToAval :: IVal -> AVal
ivalToAval (ILoc loc) = ALoc (ilocToAloc loc)
ivalToAval (IImm n) = AImm n
{-
findIGHelp :: [(Set.Set ILoc, Set.Set Rule, IAsm)] -> InterferenceGraph -> InterferenceGraph
findIGHelp ((live1, rules1, line1):(live2, rules2, line2):xs) mapping =
  let
    newmap = addInterferences line1 (map getLoc (Set.toList (Set.filter (isDef) rules1))) live2 mapping
  in
    findIGHelp ((live2, rules2, line2):xs) newmap
findIGHelp _ mapping = mapping


addInterferences :: IAsm -> [ILoc] -> Set.Set ILoc -> InterferenceGraph -> InterferenceGraph
addInterferences (IAsm [dest] op src) defs nextlive mapping =
  let
    newmapping = specialInterferences op src mapping
    toIgnore =
      case (op, src) of
        (ANop, ILoc loc) -> Set.fromList [dest, loc]
        _ -> Set.singleton dest
  in
    addInterferencesHelp toIgnore defs nextlive newmapping
addInterferences _ _ _  mapping = mapping

-- To Ignore, Defined Locations, Next line's live variables, current mapping, to new mapping
addInterferencesHelp :: Set.Set ILoc -> [ILoc] -> Set.Set ILoc -> InterferenceGraph -> InterferenceGraph
addInterferencesHelp _ [] _ m = m
addInterferencesHelp ignore (x:xs) liveVars current =
  let
    oldset = Map.findWithDefault (Set.empty) x current -- Variables that we already know that dest interferes with
    newelems = Set.difference liveVars ignore
    newset = Set.union oldset newelems
  in
    addInterferencesHelp ignore xs liveVars (Map.insert x newset current)

specialInterferences :: AOp -> IVal -> InterferenceGraph -> InterferenceGraph
specialInterferences ADiv (ILoc loc) m = specialInterferences AMod (ILoc loc) m
specialInterferences AMod (ILoc loc) m =
  let
    oldset = Map.findWithDefault (Set.empty) loc m
    updated = Set.union oldset (Set.fromList [IReg 0, IReg 1])
  in
    Map.insert loc updated m
specialInterferences _ _ m = m

heurInvCmp :: IG.Gr ILoc () -> Map.Map ALoc Int -> IG.Node -> IG.Node -> Ordering
heurInvCmp g m n1 n2 =
  let
    n1score = orderingHeuristic (G.deg g n1) (Map.findWithDefault 0 (interToAbs (fromJust (G.lab g n1))) m)
    n2score = orderingHeuristic (G.deg g n2) (Map.findWithDefault 0 (interToAbs (fromJust (G.lab g n2))) m)
  in
    compare n2score n1score

orderingHeuristic :: Int -> Int -> Int
orderingHeuristic degree loopval =
  degree + loopval

interToAbs :: ILoc -> ALoc
interToAbs (IReg t) = AReg t
interToAbs (ITemp t) = ATemp t
interToAbs (IMem t) = AMem t
interToAbs (IArg t) = AArg t
interToAbs (IInArg t) = AInArg t


spillAll :: Set.Set ILoc -> Coloring
spillAll s =
  let
    locList = Set.toList s
  in
    Map.fromList $ assignSpillRegisters locList 12

assignSpillRegisters :: [ILoc] -> Int -> [(ILoc, Int)]
assignSpillRegisters [] i = []
assignSpillRegisters (x:xs) i =
  case x of
    IReg r -> (x, r):(assignSpillRegisters xs (i))
    ITemp _ -> (x, i):(assignSpillRegisters xs (i+1))
-}
