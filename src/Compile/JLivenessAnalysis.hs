module Compile.JLivenessAnalysis where

import Compile.Types
import Compile.Types.JumpAbstractAssembly
import qualified Data.Set as Set
import qualified Data.Map as Map
import Compile.Dataflow
import Compile.RegisterList
import qualified Data.Vector.Mutable as M
import Control.Monad.ST
import Control.Monad

import qualified Data.Vector as V

findLiveSets :: V.Vector (JAsm, [Int], [Int]) -> [(Set.Set JLoc, Set.Set JRule, JAsm)]
findLiveSets progFlow =
  let
    (vectProg, vectSucc, vectPred) = V.unzip3 progFlow
    vectUseDef = V.map findUsedAndDefined vectProg
    len = V.length vectProg
    startingLivesetsVect = V.fromList (map (\i -> Set.empty) [0 .. (len - 1)])
    vectLiveSets = livenessMonadDriver startingLivesetsVect vectPred vectUseDef
  in
    V.toList $ V.zip3 vectLiveSets vectUseDef vectProg

-- Our liveness analysis is a slight mess of monads, so I'll explain what's going on.

{- This is the main blackbox function.  It takes in non-monadic arguments and gives a pure vector back.
   It does runST (creates a state), makes a mutable copy of the vector, and then seeds each line of the
   mutable vector.  Everything it calls returns unit, but writes to that mutable vector.  At the end, it
   returns a frozen copy of the mutated vector. -}
livenessMonadDriver :: V.Vector (Set.Set JLoc) -> V.Vector [Int] -> V.Vector (Set.Set JRule) -> V.Vector (Set.Set JLoc)
livenessMonadDriver startingLiveness predecessors vectUseDef =
  runST $ do
    workingVect <- V.thaw startingLiveness
    forM_ (reverse [0 .. (V.length predecessors) - 1]) (seedLine predecessors vectUseDef workingVect)
    V.freeze workingVect

{- Seedline takes in the predecessors, a vector of used and defined rules, and the working vector.  These
   never change in any run of the module, except that the working vector is mutable (so its contents are
   changing, but the vector itself is constant.)  It also takes the line number to seed.  It determines
   what variables are used at that line, and propagates each of them in sequence.  It modifies the "workingVect"
   it is given. -}  
seedLine preds vectUseDef workingVect linenum = do
  knownLive <- M.read workingVect linenum
  forM_ (Set.toList (Set.difference lineUse knownLive)) (propagate preds vectUseDef workingVect linenum) 
  where
    lineUse :: Set.Set JLoc
    lineUse = Set.map getLoc $ Set.filter isUse $ vectUseDef V.! linenum

{- Propagate takes the same constant arguments as seedline.  It takes a line number and a variable as well.
   propagate will check if it's appropriate to make the given variable live at that line (not defined,
   not already known to be live), and makes it live if so, recursively propagating to its parents.  If not,
   it'll just stop.  It doesn't really return anything useful, it just mutates workingVect. -}
propagate preds vectUseDef workingVect linenum var =
  do
    knownLive <- M.read workingVect linenum
    if (Set.member var knownLive) || (Set.member (JDef var) (vectUseDef V.! linenum)) then return () 
      else 
        do
          M.write workingVect linenum (Set.insert var knownLive) 
          forM_ (preds V.! linenum) (\line -> propagate preds vectUseDef workingVect line var) 


-- Determines used and defined rules for a given line
findUsedAndDefined :: JAsm -> Set.Set JRule
findUsedAndDefined (JJmp _ _ RJump) = Set.empty
findUsedAndDefined (JJmp _ _ _) = Set.singleton (JUse (JReg (fromIntegral eflagsRegisterNumber) 0 CNoType))
findUsedAndDefined (JAsm [dest] op args) =
  let
    specials = specialBinOps op
  in
    Set.fromList $ (useDefDest dest) ++ (concatMap useDefSrc args) ++ specials
findUsedAndDefined (JRet loc) = Set.fromList $ (useDefSrc loc)
findUsedAndDefined (JCall dest _ args) = 
  Set.fromList $ (JDef (JReg 0 0 CNoType)) : ((useDefDest dest) ++ (concatMap useDefSrc args))
findUsedAndDefined _ = Set.empty

useDefDest :: JLoc -> [JRule]
useDefDest (JHeap val _) = 
  case val of
    JLoc (JReg n 0 _) -> [JUse (JReg n 0 CNoType)]
    JLoc (JTemp n 0 _) -> [JUse (JTemp n 0 CNoType)]
    JLoc _ -> error ("Invalid address location1: " ++ show val)
    _ -> []
useDefDest loc =
  [JDef loc]

useDefSrc :: JVal -> [JRule]
useDefSrc (JLoc (JHeap val _)) =
  case val of
    JLoc (JReg n 0 _) -> [JUse (JReg n 0 CNoType)]
    JLoc (JTemp n 0 _) -> [JUse (JTemp n 0 CNoType)]
    JLoc _ -> error ("Invalid address location2:" ++ show val)
    _ -> []
useDefSrc (JLoc loc) = [JUse loc]
useDefSrc _ = []

-- Adds rules for div and mod, should be extended if we find other special binary operations
specialBinOps :: AOp -> [JRule]
specialBinOps ADiv = [JDef (JReg 0 0 CNoType), JDef (JReg 3 0 CNoType), JUse (JReg 0 0 CNoType), JUse (JReg 3 0 CNoType)]-- Uses and defines eax, edx
specialBinOps AMod = [JDef (JReg 0 0 CNoType), JDef (JReg 3 0 CNoType), JUse (JReg 0 0 CNoType), JUse (JReg 3 0 CNoType)]-- Uses and defines eax, edx
specialBinOps AShiftL = [JUse (JReg 2 0 CNoType)] -- Uses ecx
specialBinOps AShiftR = [JUse (JReg 2 0 CNoType)] -- Uses ecx
specialBinOps ACmp = [JDef (JReg (fromIntegral eflagsRegisterNumber) 0 CNoType)]
specialBinOps _  = []

-- Is the rule a use rule?
isUse :: JRule -> Bool
isUse (JUse _) = True
isUse (JDef _) = False

-- Gets the location from a rule
getLoc (JDef loc) = loc
getLoc (JUse loc) = loc