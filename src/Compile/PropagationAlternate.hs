module Compile.Propagation where
import qualified Data.Map as Map
import qualified Compile.MapWrap as MapWrap
import Compile.Types.Ops
import Compile.Types.EAST as E
import Compile.Types.TAST as T
import Compile.Types.AbstractAssembly 
import Compile.Types.JumpAbstractAssembly
import Compile.Elaboration (precompute)

type Status = BeforeCall
           |AfterCall
type State = (Status, Set.Set JLoc)
callerSaveRegs = [5,
  4,
  3,
  2,
  7,
  8,
  9,
  10]
-- TODO: Handle heap
-- TODO: Have labels put identities into the map

initialRegisters = map (\n -> JReg n 0 T.CAny) [0..14]

initialMap = Map.fromList (zip initialRegisters
  (map(\x -> JLoc x) initialRegisters))

copyConstantProp :: [JAsm] ->[JAsm]
copyConstantProp program = copyConstantPropHelp program initialMap

copyConstantPropHelp :: [JAsm] -> Map.Map JLoc JVal -> [JAsm]
copyConstantPropHelp [] m = []
copyConstantPropHelp (line:rest) m =
  case line of
    JAsm [dest] ANopDontPropagate [src] ->
      let
        newMap = Map.insert dest (JLoc dest) m
      in
        (JAsm [dest] ANop [src]):(copyConstantPropHelp rest newMap)
    JAsm [JHeap v t] ANop [src] -> -- Always write to memory locations
      let
        updatedSrc = updateSrc m src
        updatedAddress = getValFromMapping m v
      in
        (JAsm [JHeap updatedAddress t] ANop [updatedSrc]):(copyConstantPropHelp rest m)
    JAsm [dest] ANop [JLoc (JHeap v t)] -> -- If we're reading from memory, we want to only do so once if possible, so we don't optimize this away
      let
        updatedSrc = updateSrc m (JLoc (JHeap v t))
        newMap = Map.insert dest (JLoc dest) m
      in
        (JAsm [dest] ANop [updatedSrc]):(copyConstantPropHelp rest newMap)
    JAsm [dest] ANop [src] ->
      let
        updatedSrc = updateSrc m src
        newMap = Map.insert dest updatedSrc m
      in
        copyConstantPropHelp rest newMap
    JAsm [dest] ACmp srcs ->
      let
        updatedSrcs@[src1, src2] = map (updateSrc m) srcs
        -- jump@(JJmp target params jtype) = fst rest
        -- precomputed = precomputeCmp updatedSrcs jtype
        precomputed = Nothing
        emit =
          case (precomputed, src1) of
            (Nothing, JImm v) -> [JAsm [(JReg 14 0 T.CAny)] ANop [src1], JAsm [dest] ACmp [(JLoc $ JReg 14 0 T.CAny), src2]]
            (Nothing, _) -> [JAsm [dest] ACmp updatedSrcs]
      in
        emit++(copyConstantPropHelp rest m)
    JAsm [JHeap v t] op srcs -> -- Again, always write.
      let
        updatedSrcs = map (updateSrc m) srcs
        precomputed = attemptPrecompute updatedSrcs op
        updatedAddress = getValFromMapping m v
      in
        case precomputed of
          Nothing -> (JAsm [JHeap updatedAddress t] op updatedSrcs):(copyConstantPropHelp rest m)
          Just v -> (JAsm [JHeap updatedAddress t] ANop [JImm v]):(copyConstantPropHelp rest m)  
    JAsm [dest] op srcs ->
      let
        updatedSrcs = map (updateSrc m) srcs
        precomputed = attemptPrecompute updatedSrcs op
        newMap =
          case precomputed of
            Nothing -> Map.insert dest (JLoc dest) m
            Just v -> Map.insert dest (JImm v) m
      in
        case precomputed of
          Nothing -> (JAsm [dest] op updatedSrcs):(copyConstantPropHelp rest newMap)
          Just _ -> copyConstantPropHelp rest newMap
    JRet ret ->
      (JRet (updateSrc m ret)):(copyConstantPropHelp rest m)
    JLabel label vars ->
      let
        newMap = Map.union m $ Map.fromList (map (\loc -> (loc, JLoc loc)) vars)
      in
        line:(copyConstantPropHelp rest newMap)
    JJmp target params jtype ->
      let
        newLine = JJmp target (map (updateSrc m) params) jtype
      in
        newLine:(copyConstantPropHelp rest m)
    JCall dest func args ->
      let
        newArgs = map (updateSrc m) args
        newMap = Map.insert dest (JLoc dest) m
      in
        (JCall dest func newArgs):(copyConstantPropHelp rest newMap)

-- updateSrc takes in a map and a jval representing a src and gives back what the src
-- should be updated to.  Using this prevents us from trying to preprocesses/store heap values
updateSrc :: Map.Map JLoc JVal -> JVal -> JVal
updateSrc m v =
  case v of
    (JLoc (JHeap v t)) -> JLoc (JHeap (getValFromMapping m v) t)
    _ -> getValFromMapping m v

-- getValFromMapping lets you get the value of any JVal known so far.  Basically handles
-- lookups for JImms without making you case yourself.
getValFromMapping :: Map.Map JLoc JVal -> JVal -> JVal
getValFromMapping m v =
  case v of
    JImm val -> v
    JLoc loc -> getLocFromMapping m loc

-- JInArg is not allowed to be taken!
getLocFromMapping :: Map.Map JLoc JVal -> JLoc -> JVal
getLocFromMapping m loc@(JInArg _ _ _) = JLoc loc
getLocFromMapping m loc@(JReg   _ _ _) = m Map.! loc
getLocFromMapping m loc@(JTemp  _ _ _) = m Map.! loc
getLocFromMapping m loc@(JMem   _ _ _) = m Map.! loc
getLocFromMapping m loc@(JArg   _ _ _) = m Map.! loc
getLocFromMapping m loc@(JHeap  _ _)   = m Map.! loc

attemptPrecompute :: [JVal] -> AOp -> Maybe Int
attemptPrecompute [(JImm n1), (JImm n2)] op =
  let
    expr = precompute (convertAOpToBinop op) n1 n2 
  in
    case expr of
      E.Const v -> Just v
      _ -> Nothing
attemptPrecompute _ _ = Nothing

convertAOpToBinop :: AOp -> Binop
convertAOpToBinop op =
  case op of
    AAdd -> Add
    AAddq -> Add
    ASub -> Sub
    ASubq -> Sub
    ADiv -> Div
    AMul -> Mul 
    AMod -> Mod 
    AShiftL -> ShiftL
    AShiftR -> ShiftR
    ALess -> Less
    ALeq -> Leq
    AGeq -> Geq
    AGreater -> Greater
    AEq -> Eq
    ANeq -> Neq
    ABAnd -> BAnd
    ABXor -> BXor
    ABOr -> BOr 
    ALAnd -> LAnd
    ALOr -> LOr
    ACmpq -> LOr -- These two lines are horrible hacks
    ACmp -> LOr -- because elaboration will not precompute with LOr.  



