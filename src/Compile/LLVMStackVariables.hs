module Compile.LLVMStackVariables(stackifyFdecl) where
import Compile.Types.LLVM
import qualified Data.Set as Set
import Debug.Trace

type StackifyState = Int --Describes the next temp we can use

stackifyFdecl :: LFdecl -> LFdecl
stackifyFdecl (LFdecl t name args prog) = 
  let
    newProg = (stackifyProg prog args) ++ [LStr "unreachable"]
  in
    LFdecl t name args newProg

stackifyProg :: [LInstr] -> [LLoc] -> [LInstr]
stackifyProg l args = 
  let
    initState = 1 -- NOT 0!!!!!!!!!!!
    foundTemps = findTemps l
    minusArgs = Set.toList (Set.difference (Set.fromList foundTemps) (Set.fromList args))
    filterTemps = filter isTemp minusArgs
    filterTempsWithTypes = filter hasType filterTemps
    allocInstrList = allocStacks (Set.toList $ Set.fromList $ filterTempsWithTypes)
    (stackified,_) = stackifyInstrList args initState l
  in
    allocInstrList ++ stackified

findTemps :: [LInstr] -> [LLoc]
findTemps (instr:l) =
  case instr of
    (LInstr dest@(LTemp n t) op src1 src2) -> dest:src1:src2:(findTemps l)
    (LIcmp dest@(LTemp n t) cmp src1 src2) -> dest:src1:src2:(findTemps l)
    (LAlloca dest@(LTemp n tdest) t) -> dest:(findTemps l)
    (LCall dest@(LTemp n t) func srcs) -> srcs ++ (dest:(findTemps l))
    (LLoad dest@(LTemp n t) src) -> dest:src:(findTemps l)
    (LBitcast dest@(LTemp n t) src targetType) -> dest:src:(findTemps l)
    (LStore dest src) -> dest:src:(findTemps l)
    (LBrCond cond iflabel elselabel) -> (typifyLoc LBool cond):(findTemps l)
    (LBr label) -> (findTemps l)
    (LLabel label) -> (findTemps l)
    (LMove dest@(LTemp n t) src) -> dest:src:(findTemps l)
    (LRet src) -> src:(findTemps l)
    LRetVoid -> (findTemps l)
    (LStr str) -> (findTemps l)
    LIf cond ifbr elsebr -> (findTemps ifbr) ++ (findTemps elsebr) ++ (cond:findTemps l)
    LWhile cond body -> (findTemps body) ++ (cond:findTemps l)
    LGetelementptrInstr dest src args -> (dest:src:args) ++ (findTemps l)
    thingy -> error("Missing findTemps case:" ++ show thingy)
findTemps [] = []

allocStacks :: [LLoc] -> [LInstr]
allocStacks locs = map allocStack $ filter (\loc -> (getType loc) /= LVoid) locs

allocStack :: LLoc -> LInstr
allocStack loc =
  let
    t = getType loc
  in
    (LAlloca loc t)

stackifyInstrList :: [LLoc] -> StackifyState -> [LInstr] -> ([LInstr], StackifyState)
stackifyInstrList args state (instr:l) =
  let
    (newinstr,newState) = stackifyInstr args state instr
    (finalinstr, finalState) = stackifyInstrList args newState l
  in
    (newinstr ++ finalinstr, finalState)
stackifyInstrList args state [] = ([],state)

stackifyInstr :: [LLoc] -> StackifyState -> LInstr -> ([LInstr], StackifyState)
stackifyInstr args state instr =
  let
    r = state
  in
  case instr of
    (LInstr dest@(LTemp n t) op src1 src2) ->
      let
        (rsrc1, movesrc1) = (makesrc args r src1)
        (rsrc2, movesrc2) = (makesrc args (r+1) src2)
        (rdest, movedest) = (makedest args (r+2) dest)
      in
        (movesrc1 ++ movesrc2 ++
          [LInstr rdest op rsrc1 rsrc2] ++ movedest,
          r+3
        )
    (LIcmp dest cmp src1 src2) ->
      let
        t = getType src1
      in
        case t of
          (LPtr _) ->
            let
              (rsrc1, movesrc1) = (makesrc args r src1)
              (rsrc2, movesrc2) = (makesrc args (r+1) src2)
              (rdest, movedest) = (makedest args (r+2) dest)
              cast1 = LTemp (r+3) (LPtr LInt8)
              cast2 = LTemp (r+4) (LPtr LInt8)
            in
              (movesrc1 ++ movesrc2 ++
                [LBitcast cast1 rsrc1 (LPtr LInt8),
                  LBitcast cast2 rsrc2 (LPtr LInt8),
                  LIcmp rdest cmp cast1 cast2] ++
                movedest,
                r+5
              )
          _ ->
            let
              (rsrc1, movesrc1) = (makesrc args r src1)
              (rsrc2, movesrc2) = (makesrc args (r+1) src2)
              (rdest, movedest) = (makedest args (r+2) dest)
            in
              (movesrc1 ++ movesrc2 ++
                [LIcmp rdest cmp rsrc1 rsrc2] ++ movedest,
                r+3
              )
    (LAlloca dest@(LTemp n tdest) t) -> ([LAlloca dest t],r)
    (LCall dest@(LTemp n t) func srcs) -> 
      let
        (rdest, movedest) = (makedest args (r) dest)
        (rsrcs, moveSrcsRaw) = unzip (map (\(s,i) -> makesrc args i s) (zip srcs [(r+1)..]))
        moveSrcs = concat moveSrcsRaw
      in (moveSrcs ++ [LCall rdest func rsrcs] ++ movedest
          ,r + 1 + (length rsrcs)
        )
    LGetelementptrInstr dest srcstruct srcs ->
      let
        (rdest, movedest) = (makedest args (r) dest)
        (rsrcstruct, movesrc) = (makesrc args (r+1) dest)
        (rsrcs, moveSrcsRaw) = unzip (map (\(s,i) -> makesrc args i s) (zip srcs [(r+2)..]))
        moveSrcs = concat moveSrcsRaw
      in (moveSrcs ++ movesrc ++ [LGetelementptrInstr rdest rsrcstruct rsrcs] ++ movedest
          ,r + 2 + (length rsrcs)
        )
    (LLoad dest@(LTemp n t) src1) ->
      let
        (rsrc1, movesrc1) = (makesrc args r src1)
        (rdest, movedest) = (makedest args (r+1) dest)
      in
        (movesrc1 ++ [LLoad rdest rsrc1] ++ movedest,
          r+2
        )
    (LBitcast dest@(LTemp n t) src1 targetType) ->
      let
        (rsrc1, movesrc1) = (makesrc args r src1)
        (rdest, movedest) = (makedest args (r+1) dest)
      in
        (movesrc1 ++ [LBitcast rdest rsrc1 targetType] ++ movedest,
          r+2
        )
    (LStore dest src1) ->
      let
        (rsrc1, movesrc1) = (makesrc args r src1)
        (rsrc1x, movesrc1x) = (transsrcStore dest args (r+2) rsrc1)
        -- Dest is really a pointer that will be USED by the STORE instruction
        -- So use makesrc
        (rdest, movedest) = (makesrc args (r+1) dest)
      in
        (movesrc1 ++ movesrc1x ++ movedest ++ [LStore rdest rsrc1x],
          r+3
        )
    (LBrCond src1 iflabel elselabel) ->
      let
        (rsrc1, movesrc1) = (makesrc args r (typifyLoc LBool src1))
      in
        (movesrc1 ++ [LBrCond rsrc1 iflabel elselabel],
          r+1
        )
    (LBr label) -> ([LBr label],r)
    (LMove dest@(LTemp n t) src1) ->
      let
        (rsrc1, movesrc1) = (makesrc args r src1)
        (rsrc1x, movesrc1x) = (transsrc dest args (r+2) rsrc1)
        (rdest, movedest) = (makedest args (r+1) dest)
      in
        (movesrc1 ++ movesrc1x ++ [LMove rdest rsrc1x] ++ movedest,
          r+3
        )
    (LRet src1) -> 
      let
        (rsrc1, movesrc1) = (makesrc args r src1)
      in
        ([LLoad rsrc1 (src1),
          LRet rsrc1
          ],
          r+1
        )
    LRetVoid -> ([LRetVoid],r)
    (LStr str) -> ([LStr str],r)
    LIf src1 ifbr elsebr -> 
      let
        (rsrc1, movesrc1) = (makesrc args r src1)
        (sifbr,r2) = stackifyInstrList args (r+1) ifbr
        (selsebr, r3) = stackifyInstrList args (r2) elsebr
      in
        (movesrc1 ++ [LIf rsrc1 sifbr selsebr], r3)
    LWhile src1 body ->
      let
        (rsrc1, movesrc1) = (makesrc args r src1)
        (sbody,r2) = stackifyInstrList args (r+1) body
      in
        (movesrc1 ++ [LWhile rsrc1 (sbody ++ movesrc1)], r2)
    (LLabel label) -> ([LLabel label],r)

transsrc dest args r src = 
  case ((getType src) == LPtr LVoid) of
    True ->
      let
        t = getType dest
        rsrc = scratch r t
      in
        (rsrc, [LBitcast rsrc src t])
    False -> (src, [])

transsrcStore dest args r src = 
  case ((getType src) == LPtr LVoid) of
    True ->
      let
        (LPtr t) = getType dest
        rsrc = scratch r t
      in
        (rsrc, [LBitcast rsrc src t])
    False -> (src, [])


makesrc args r src@(LTemp n t) = 
  case (elem src args || t == LVoid) of
    True -> (src,[])
    False ->
      let
        rsrc = scratch r t
      in
        (rsrc, [LLoad rsrc src])
makesrc args r src = (src,[])

makedest args r dest@(LTemp n t) = 
  case (elem dest args || t == LVoid) of
    True -> (dest,[])
    False ->
      let
        rdest = scratch r t
      in
        (rdest, [LStore dest rdest])
makedest args r dest = (dest,[])


scratch r ltype = LTemp (-r) ltype

getType loc = getLLocType loc

isTemp (LTemp _ _) = True
isTemp _ = False

hasType t@(LTemp _ LReserved) = trace ("WarningLCK: LTemp has no type:" ++ show t) $ False
hasType (LTemp _ _) = True

isReserved LReserved = True
isReserved _ = False
