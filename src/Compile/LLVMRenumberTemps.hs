module Compile.LLVMRenumberTemps(renumberFdecl) where

type VarLabelState = (Int,Int)

renumberFdecl :: VarLabelState -> LFdecl -> (LFdecl, VarLabelState)
renumberFdecl s (LFdecl t name args prog) = 
  let
    newProg = map (renumberInstr s) prog
    newArgs = map (renumberArg s) args
    newState = getNewState newProg
  in
    (LFdecl t name args newProg, newState)

renumberInstr :: VarLabelState -> LInstr -> LInstr
renumberInstr s instr =
	case instr of
		(LInstr dest op src1 src2) -> (LInstr (renum s dest) op (renum s (renum s src1)) (renum s src2))
		(LIcmp dest cmp src1 src2) -> (LIcmp (renum s dest) cmp (renum s (renum s src1)) (renum s src2))
		(LAlloca dest t) -> (LAlloca (renum s dest) t)
		(LCall dest func srcs) -> (LCall (renum s dest) func (map (renum s) srcs))
		(LLoad dest src) -> (LLoad (renum s dest) (renum s src))
		(LBitcast dest src targetType) -> (LBitcast (renum s dest) (renum s src) targetType)
		(LStore dest src) -> (LStore (renum s dest) (renum s src))
		(LBrCond cond iflabel elselabel) -> (LBrCond (renum s cond) iflabel elselabel)
		(LBr label) -> (LBr label)
		(LLabel label) -> (LLabel label)
		(LMove dest src) -> (LMove (renum s dest) (renum s src))
		(LRet src) -> (LRet (renum s src))
		LRetVoid -> LRetVoid
		(LStr str) -> (LStr str)
		LIf cond ifbr elsebr -> LIf (renum s cond) (map (renumberInstr s) ifbr) (map (renumberInstr s) elsebr)
		LWhile cond body -> LWhile (renum s cond) (map (renumberInstr s) body)

getNewState =
	foundTemps = (Set.toList $ Set.fromList $ findTemps l)
	filterTemps = filter isTemp foundTemps
	tempNums = map (\(LTemp n _) -> n) filterTemps
	largest = maximum (0:tempNums)
	smallest = minimum (0:tempNums)
	in
		(largest + 1, abs(smallest) + 1)

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
    thingy -> error("Missing findTemps case:" ++ show thingy)
findTemps [] = []

isTemp (LTemp _ _) = True
isTemp _ = False


