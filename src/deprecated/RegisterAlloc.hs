module Compile.RegisterAlloc (registerAlloc) where

import Compile.Types
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive as IG

registerAlloc :: [IAsm] -> Int -> RegisterColoring
registerAlloc l n =
    let
        lrev = reverse l
        emptySet = Set.empty
        setList = reverse (livenessAnalysis lrev emptySet)

        nodes :: [G.LNode Int]
        nodes = [(x,0) | x <- [0..(n-1)]]

        edges :: [G.LEdge ()]
        edges = map (\(x,y) -> (x,y,())) (getEdgesForInteferenceGraph l setList)

        interferenceGraph :: IG.Gr Int ()
        interferenceGraph = G.mkGraph nodes edges

        degrees = [0 | x <- [0..(n-1)]]
    in
        Map.empty

-- requires a reversed [IAsm]
-- the user must reverse it again at the end
livenessAnalysis :: [IAsm] -> Set.Set Int -> [Set.Set Int]
livenessAnalysis [] set = [set]
livenessAnalysis (x:xs) set =
    let
        preSet = livenessForLine x set
    in
        preSet : (livenessAnalysis xs preSet)

livenessForLine :: IAsm -> Set.Set Int -> Set.Set Int
-- Unary op
-- two registers
livenessForLine (IAsm [ITemp dst] ANop (ILoc (ITemp src))) set =
    let
        setNoDst = Set.delete dst set
        setWithSrc = Set.insert src setNoDst
    in
        setWithSrc
-- one register, one immediate
livenessForLine (IAsm [ITemp dst] ANop (IImm n)) set = Set.delete dst set
-- binary op
-- two registers
livenessForLine (IAsm [ITemp dst] op (ILoc (ITemp src))) set =
    let
        setWithDst = Set.insert dst set
        setWithSrc = Set.insert src setWithDst
    in
        setWithSrc
-- one register, one immediate
livenessForLine (IAsm [ITemp dst] op (IImm n)) set = Set.insert dst set
-- return statements, one in register
livenessForLine (IRet (ILoc (ITemp retval))) set = Set.insert retval set
-- return statements, immediate
livenessForLine (IRet (IImm _)) set = set
-- fail
livenessForLine _ set = error "RegisterAlloc: No rule matched Liveness Analysis"

getEdgesForInteferenceGraph :: [IAsm] -> [Set.Set Int] -> [(Int, Int)]
getEdgesForInteferenceGraph _ [] = []
getEdgesForInteferenceGraph [] _ = error "RegisterAlloc: IAsm and set size does not match"
getEdgesForInteferenceGraph (x:xs) (set:ys) =
    let 
        l = case x of 
            (IAsm [ITemp dst] ANop (ILoc (ITemp src))) -> findInterferencePerElement (\x -> (x /= dst) && (x /= src)) dst set
            (IAsm [ITemp dst] ANop (IImm n)) -> findInterferencePerElement (\x -> x /= dst) dst set
            (IAsm [ITemp dst] op _) -> findInterferencePerElement (\x -> x /= dst) dst set
            _ -> error "RegisterAlloc: unknown case"
    in
        l ++ (getEdgesForInteferenceGraph xs ys)


findInterferencePerElement :: (Int -> Bool) -> Int -> Set.Set Int -> [(Int, Int)]
findInterferencePerElement f e set = 
    map (\x -> (e, x)) (Set.toList (Set.filter f set))








