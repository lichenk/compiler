module Compile.MakeSSA where

import qualified Compile.MapWrap as MapWrap
import Compile.Types.AbstractAssembly
import Compile.Types.Ops
import Compile.Types.TAST
import qualified Data.Map as Map
import Debug.Trace

-- Wrapper function for makeSSAhelp
-- Takes a program in Three-Address Abstract Assembly and gives a
-- Three-Address Abstract Assembly program in SSA form
makeSSA :: [AAsm] -> [AAsm]
makeSSA l = 
  let
    (newl, _, _) = makeSSAhelp l Map.empty 0
  in
    newl

-- Iterates through the list, applying convertLine at each step, and passing on
-- the new mapping and the count of how many variables have been used
makeSSAhelp :: [AAsm] -> Map.Map Int Int -> Int -> ([AAsm], Map.Map Int Int, Int)
makeSSAhelp [] mapping next_value = ([], mapping, next_value)
makeSSAhelp (l:ls) mapping next_value = 
  let
    (newL, newmapping, newcount) = 
      case l of 
        AIf _ _ _ -> convertIf l mapping next_value
        AWhile _ _ -> convertWhile l mapping next_value
        ACall _ _ _ -> convertCall l mapping next_value
        _ -> convertLine l mapping next_value
    (restOfList, retMapping, retCount) = makeSSAhelp ls newmapping newcount
  in
    (newL : restOfList, retMapping, retCount)

convertCall :: AAsm -> Map.Map Int Int -> Int -> (AAsm, Map.Map Int Int, Int)
convertCall (ACall (ATemp t mytype) op srcs) mapping next_value =
  let
    new_srcs = map (convertSrc mapping) srcs
    (new_dest, new_map, new_next_value) =
      (ATemp next_value mytype, Map.insert t next_value mapping, next_value + 1)
  in
    (ACall new_dest op new_srcs, new_map, new_next_value)
convertCall (ACall dest op srcs) mapping next_value =
  let
    mytype = getLocType dest
    newDest = 
      case getBasicLoc dest of
        Nothing -> dest
        Just n -> AHeap (ALoc (ATemp (mapping Map.! n) (CPtr mytype))) mytype
    new_srcs = map (convertSrc mapping) srcs
  in
    (ACall newDest op new_srcs, mapping, next_value)

-- Will fail if passed anything other than AIf.  This is expected.
convertIf :: AAsm -> Map.Map Int Int -> Int -> (AAsm, Map.Map Int Int, Int)
convertIf (AIf cond ifprogram elseprogram) mapping count =
  let
    (ifssa, ifmap, ifcount) = makeSSAhelp ifprogram mapping count
    (elsessa, elsemap, elsecount) = makeSSAhelp elseprogram mapping ifcount
    newcount = max ifcount elsecount
    newmapping = Map.unionWith max ifmap elsemap
    ifupdates = conditionalUpdates newmapping ifmap
    elseupdates = conditionalUpdates newmapping elsemap
    newifprog = ifssa ++ ifupdates
    newelseprog = elsessa ++ elseupdates
    newcond = -- here we assume that the conditional will be in a temp.  Seems reasonable.
      case cond of
        ACond val -> ACond (convertVal mapping val)
        ACondOptimized val1 op val2 -> ACondOptimized (convertVal mapping val1) op (convertVal mapping val2)
  in
    (AIf newcond newifprog newelseprog, newmapping, newcount)


-- Converts a val. For converting conds. Very restrictive
convertVal mapping (ALoc (ATemp n ctype)) =
  ALoc (ATemp (mapping Map.! n) ctype)
convertVal mapping val = val

-- Will fail if passed anything other than AWhile.  This is expected.
convertWhile :: AAsm -> Map.Map Int Int -> Int -> (AAsm, Map.Map Int Int, Int)
convertWhile (AWhile cond prog) mapping count =
  let
    (bodyssa, bodymap, bodycount) = makeSSAhelp prog mapping count
    updates = conditionalUpdates mapping bodymap
    newcond = -- here we assume that the conditional will be in a temp.  Seems reasonable.
      case cond of
        ACond val -> ACond (convertVal mapping val)
        ACondOptimized val1 op val2 -> ACondOptimized (convertVal mapping val1) op (convertVal mapping val2)
  in
    ((AWhile newcond (bodyssa ++ updates)), mapping, bodycount)
-- Converts a single line of assembly to SSA form, given a mapping and the next
-- number to be used.
-- Note that convertLine currently does *nothing* if there are
-- multiple dests
convertLine :: AAsm -> Map.Map Int Int -> Int -> (AAsm, Map.Map Int Int, Int)
convertLine (AAsm [ATemp t mytype] op srcs) mapping next_value =
  let
    new_srcs = map (convertSrc mapping) srcs
    (new_dest, new_map, new_next_value) =
      (ATemp next_value mytype, Map.insert t next_value mapping, next_value + 1)
  in
    (AAsm [new_dest] op new_srcs, new_map, new_next_value)
convertLine (AAsm [dest] op srcs) mapping next_value =
  let
    newDest = 
      case getBasicLoc dest of
        Just n -> AHeap (ALoc (ATemp (mapping Map.! n) (CPtr (getLocType dest)))) (getLocType dest)
        Nothing -> dest
    new_srcs = map (convertSrc mapping) srcs
  in
    (AAsm [newDest] op new_srcs, mapping, next_value)
convertLine line@(ARet (ALoc loc)) mapping next_value =
  case getBasicLoc loc of
    Just n ->
      case loc of
        AHeap _ t -> (ARet (ALoc (AHeap (ALoc (ATemp (mapping Map.! n) (CPtr t))) t)), mapping, next_value)
        ATemp _ t -> (ARet (ALoc (ATemp (mapping Map.! n) t)), mapping, next_value)
    Nothing -> (line, mapping, next_value)
convertLine code mapping next_value = (code, mapping, next_value)

convertSrc :: Map.Map Int Int -> AVal -> AVal
convertSrc mapping (ALoc (AHeap inner t)) = ALoc (AHeap (convertSrc mapping inner) t)
convertSrc mapping (ALoc (ATemp t mytype)) = 
  if Map.member t mapping then
    ALoc (ATemp (mapping Map.! t) mytype)
  else
    error ("makeSSA: A variable referenced in a right-hand expression was not in the mapping:" ++ (show t))
convertSrc _ val = val


conditionalUpdates :: Map.Map Int Int -> Map.Map Int Int -> [AAsm]
conditionalUpdates reference pathmap =
  map (\k -> AAsm [ATemp (reference Map.! k) CNoType] ANop [ALoc (ATemp (pathmap Map.! k) CNoType)]) (findMismatchedKeys pathmap reference)

-- Finds keys which have different values between the two maps.
-- If a key doesn't appear in one, it should be absent.
findMismatchedKeys :: Map.Map Int Int -> Map.Map Int Int -> [Int]
findMismatchedKeys map1 map2 =
  let
    commonKeys = Map.keys $ Map.intersection map1 map2
  in
    filter (\k -> (map1 Map.! k /= map2 Map.! k)) commonKeys

-- If there's a temp somewhere in here, it'll get its number, else, nothing
getBasicLoc :: ALoc -> Maybe Int
getBasicLoc (ATemp n _) = Just n
getBasicLoc (AHeap (ALoc loc) _) = getBasicLoc loc
getBasicLoc _ = Nothing

getLocType :: ALoc -> CType
getLocType (AReg _ t) = t
getLocType (ATemp _ t) = t
getLocType (AMem _ t) = t
getLocType (AInArg _ t) = t
getLocType (AArg _ t) = t
getLocType (AHeap _ t) = t
