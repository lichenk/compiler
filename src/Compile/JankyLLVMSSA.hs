module Compile.JankyLLVMSSA where
import Compile.Types.LLVM 
import qualified Data.Map as Map

llvmSSA :: LFdecl -> LFdecl
llvmSSA (LFdecl t name args prog) = 
  let
    (_, _, newProg) = llvmSSAHelp (addArgMoves prog args) Map.empty 0
  in
    LFdecl t name args newProg

llvmSSAHelp :: [LInstr] -> Map.Map Int Int -> Int -> (Int, Map.Map Int Int, [LInstr])
llvmSSAHelp [] m next = (next, m, [])
llvmSSAHelp (x:xs) m next =
  let 
    (newN, newM, newLine) =
      case x of
        (LInstr dest@(LTemp n t) op src1 src2) ->
          let
            newDest = LTemp next t
            newMap = Map.insert n next m
          in
            (next+1, newMap, LInstr newDest op (convertLoc m src1) (convertLoc m src2))
        (LIcmp dest@(LTemp n t) cmp src1 src2) ->
          let
            newDest = LTemp next t
            newMap = Map.insert n next m
          in
            (next+1, newMap, LIcmp newDest cmp (convertLoc m src1) (convertLoc m src2))
        (LAlloca dest@(LTemp n tdest) t) ->
          let
            newDest = LTemp next tdest
            newMap = Map.insert n next m
          in
            (next+1, newMap, LAlloca newDest t)
        (LCall dest@(LTemp n t) func srcs) ->
          let
            newDest = LTemp next t
            newMap = Map.insert n next m
            newSrcs = map (convertLoc m) srcs
          in
            (next+1, newMap, LCall newDest func newSrcs)
        (LLoad dest@(LTemp n t) src) ->
          let
            newDest = LTemp next t
            newMap = Map.insert n next m
          in
            (next+1, newMap, LLoad newDest (convertLoc m src))
        (LBitcast dest@(LTemp n t) src targetType) ->
          let
            newDest = LTemp next t
            newMap = Map.insert n next m
          in
            (next+1, newMap, LBitcast newDest (convertLoc m src) targetType)
        (LStore destPtr src) ->
          (next, m, LStore (convertLoc m destPtr) (convertLoc m src))
        (LBrCond cond iflabel elselabel) ->
          (next, m, LBrCond (convertLoc m cond) iflabel elselabel)
        (LBr label) -> (next, m, LBr label)
        (LMove dest@(LTemp n t) src) ->
          let
            newDest = LTemp next t
            newMap = Map.insert n next m
          in
            (next+1, newMap, LMove newDest (convertLoc m src))
        (LRet src) -> (next, m, LRet (convertLoc m src))
        LRetVoid -> (next, m, LRetVoid)
        (LStr str) -> (next, m, LStr str)
        LIf cond ifbr elsebr ->
          let
            newCond = convertLoc m cond
            (ifNext, ifMap, newIf) = llvmSSAHelp ifbr m next
            (elseNext, elseMap, newElse) = llvmSSAHelp elsebr ifMap ifNext
          in
            (elseNext, elseMap, LIf newCond newIf newElse)
        LWhile cond body ->
          let
            newCond = convertLoc m cond
            (bodyNext, bodyMap, newBody) = llvmSSAHelp body m next
          in
            (bodyNext, bodyMap, LWhile newCond newBody)
    (restNext, restMap, restProg) = llvmSSAHelp xs newM newN 
  in
    (restNext, restMap, newLine:restProg)

-- To make sure that the arguments aren't messed up by SSA, 
-- we move them to themselves at the beginning so that they're put into the map.
addArgMoves :: [LInstr] -> [LLoc] -> [LInstr]
addArgMoves body args =
  let
    moves = map (\loc -> LMove loc loc) args
  in
    moves ++ body

convertLoc :: Map.Map Int Int -> LLoc -> LLoc
convertLoc m (LTemp n t) = LTemp (Map.findWithDefault n n m) t
convertLoc _ (LImm n t) = LImm n t
convertLoc m (LHeap loc t) = LHeap (convertLoc m loc) t

