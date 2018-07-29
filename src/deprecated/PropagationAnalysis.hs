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

createPropagationMaps :: [IAsm] -> V.Vector (IAsm, [Int], [Int]) -> V.Vector (IAsm, Map.Map ILoc IVal)
createPropagationMaps progFlow =
  let
    (vectProg, vectSucc, vectPred) = V.unzip3 progFlow
    len = V.length vectProg
    startingPropMaps = V.generate len (\i -> Map.empty)
    vectPropMaps = propagationMonadDriver startingPropMaps vectSucc

propagationMonadDriver :: V.Vector (Map.Map ILoc IVal) -> V.Vector IAsm -> V.Vector [Int]
propagationMonadDriver startProps prog successors =
  runST $ do
    workingVect <- V.thaw startingPropMaps
    forM_ (reverse [0 .. ((V.length successors)-1)]) (seedLine successors prog workingVect)

seedLine successors prog working linenum = 
  case prog V.! linenum of
    IAsm [dest] ANop (IImm imm) ->
      do
        knownProps <- M.read workingVect linenum
        let newmap = Map.insert dest (IImm imm) knownProps
        M.write workingVect linenum newmap





findLiveSets :: [IAsm] -> V.Vector (IAsm, [Int], [Int]) -> [(Set.Set ILoc, Set.Set Rule, IAsm)]
findLiveSets prog progFlow =
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
livenessMonadDriver :: V.Vector (Set.Set ILoc) -> V.Vector [Int] -> V.Vector (Set.Set Rule) -> V.Vector (Set.Set ILoc)
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
    lineUse :: Set.Set ILoc
    lineUse = Set.map getLoc $ Set.filter isUse $ vectUseDef V.! linenum

{- Propagate takes the same constant arguments as seedline.  It takes a line number and a variable as well.
   propagate will check if it's appropriate to make the given variable live at that line (not defined,
   not already known to be live), and makes it live if so, recursively propagating to its parents.  If not,
   it'll just stop.  It doesn't really return anything useful, it just mutates workingVect. -}
propagate preds vectUseDef workingVect linenum var =
  do
    knownLive <- M.read workingVect linenum
    if (Set.member var knownLive) || (Set.member (Def var) (vectUseDef V.! linenum)) then return () 
      else 
        do
          M.write workingVect linenum (Set.insert var knownLive) 
          forM_ (preds V.! linenum) (\line -> propagate preds vectUseDef workingVect line var) 

colorOutArg :: Int -> ILoc
colorOutArg n =
  case n of
    0 -> IReg 5 CNoType --edi
    1 -> IReg 4 CNoType --esi
    2 -> IReg 3 CNoType --edx
    3 -> IReg 2 CNoType --ecx
    4 -> IReg 7 CNoType --r8
    5 -> IReg 8 CNoType --r9
    m -> IArg m CNoType


-- Determines used and defined rules for a given line
findUsedAndDefined :: IAsm -> Set.Set Rule
findUsedAndDefined (IAsm [dest] ANop arg) = Set.fromList $ (useDefDest dest False) ++ (useDefSrc arg)
findUsedAndDefined (IAsm [dest] ACmp arg) = Set.fromList $ (useDefDest dest False) ++ (useDefSrc arg) ++ [Def (IReg (fromIntegral eflagsRegisterNumber) CNoType)]
findUsedAndDefined (IAsmJump RJump _) = Set.empty
findUsedAndDefined (IAsmJump _ _) = Set.singleton (Use (IReg (fromIntegral eflagsRegisterNumber) CNoType))
findUsedAndDefined (IAsm [dest] op arg) =
  let
    specials = specialBinOps op
  in
    Set.fromList $ (useDefDest dest True) ++ (useDefSrc arg) ++ specials
findUsedAndDefined (IRet loc) = Set.fromList $ (useDefSrc loc)
findUsedAndDefined (ICall _ n) = 
  Set.union (Set.fromList (map (\i -> Use (colorOutArg (i-1))) [1.. n]))
  (Set.fromList [Def (IReg 0 CNoType)])
findUsedAndDefined _ = Set.empty


findPropRules :: IAsm -> Set.Set PropRule 
findPropRules (IAsm [dest] ANop [arg]) = Set.singleton $ PropDef dest arg
findPropRules (IAsm [dest] ACmp [arg]) = Set.empty
findPropRules (IAsmJump _ _) = Set.empty
findPropRules (IAsm [dest] )


useDefDest :: ILoc -> Bool -> [Rule]
useDefDest (IHeap val _) _ = 
  case val of
    ILoc (IReg n _) -> [Use (IReg n CNoType)]
    ILoc (ITemp n _) -> [Use (ITemp n CNoType)]
    ILoc _ -> error "Invalid address location"
    _ -> []
-- destUsed should be true iff the assignment operator is not equals
useDefDest loc destUsed =
  if destUsed then [Use loc, Def loc] else [Def loc]

useDefSrc :: IVal -> [Rule]
useDefSrc (ILoc (IHeap val _)) =
  case val of
    ILoc (IReg n _) -> [Use (IReg n CNoType)]
    ILoc (ITemp n _) -> [Use (ITemp n CNoType)]
    ILoc _ -> error "Invalid address location"
    _ -> []
useDefSrc (ILoc loc) = [Use loc]
useDefSrc _ = []

-- Adds rules for div and mod, should be extended if we find other special binary operations
specialBinOps :: AOp -> [Rule]
specialBinOps ADiv = [Def (IReg 0 CNoType), Def (IReg 3 CNoType), Use (IReg 0 CNoType), Use (IReg 3 CNoType)]-- Uses and defines eax, edx
specialBinOps AMod = [Def (IReg 0 CNoType), Def (IReg 3 CNoType), Use (IReg 0 CNoType), Use (IReg 3 CNoType)]-- Uses and defines eax, edx
specialBinOps AShiftL = [Use (IReg 2 CNoType)] -- Uses ecx
specialBinOps AShiftR = [Use (IReg 2 CNoType)] -- Uses ecx
specialBinOps _  = []

-- Is the rule a use rule?
isUse :: Rule -> Bool
isUse (Use _) = True
isUse (Def _) = False

-- Gets the location from a rule
getLoc (Def loc) = loc
getLoc (Use loc) = loc