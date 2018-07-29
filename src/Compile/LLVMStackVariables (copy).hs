module Compile.LLVMStackVariables(stackifyFdecl) where
import Compile.Types.LLVM
import qualified Data.Set as Set

type StackifyState = Int --Describes the next temp we can use

stackifyFdecl :: LFdecl -> LFdecl
stackifyFdecl (LFdecl t name args prog) = 
  let
    newProg = stackifyProg prog
  in
    LFdecl t name args newProg

stackifyProg :: [LInstr] -> [LInstr]
stackifyProg l = 
	let
		initState = 1 -- NOT 0!!!!!!!!!!!
		foundTemps = (Set.toList $ Set.fromList $ findTemps l)
		filterTemps = filter isTemp foundTemps
		filterTempsWithTypes = filter hasType filterTemps
		allocInstrList = allocStacks (Set.toList $ Set.fromList $ filterTempsWithTypes)
		(stackified,_) = stackifyInstrList initState l
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
    thingy -> error("Missing findTemps case:" ++ show thingy)
findTemps [] = []

allocStacks :: [LLoc] -> [LInstr]
allocStacks locs = map allocStack locs

allocStack :: LLoc -> LInstr
allocStack loc =
	let
		t = getType loc
	in
		(LAlloca loc t)

stackifyInstrList :: StackifyState -> [LInstr] -> ([LInstr], StackifyState)
stackifyInstrList state (instr:l) =
	let
		(newinstr,newState) = stackifyInstr state instr
		(finalinstr, finalState) = stackifyInstrList newState l
	in
		(newinstr ++ finalinstr, finalState)
stackifyInstrList state [] = ([],state)

stackifyInstr :: StackifyState -> LInstr -> ([LInstr], StackifyState)
stackifyInstr state instr =
	let
		r = state
	in
	case instr of
  	(LInstr dest@(LTemp n t) op src1 src2) ->
  		let
  			(rsrc1, movesrc1) = (makesrc r src1)
  			(rsrc2, movesrc2) = (makesrc (r+1) src2)
  			rdest = (scratch (r+2) (getType dest))
  		in
  			(movesrc1 ++ movesrc2 ++
  				[LInstr rdest op rsrc1 rsrc2,
  					LStore (makePtr dest) rdest
  				],
  				r+3
  			)
  	(LIcmp dest@(LTemp n t) cmp src1 src2) ->
  		let
  			(rsrc1, movesrc1) = (makesrc r src1)
  			(rsrc2, movesrc2) = (makesrc (r+1) src2)
  			rdest = (scratch (r+2) (getType dest))
  		in
  			(movesrc1 ++ movesrc2 ++
  				[LIcmp rdest cmp rsrc1 rsrc2,
  				LStore (makePtr dest) rdest
  				],
  				r+3
  			)
  	(LAlloca dest@(LTemp n tdest) t) -> ([LAlloca dest t],r)
  	(LCall dest@(LTemp n t) func srcs) -> 
  		let
  			rdest = (scratch r (getType dest))
  			(rsrcs, moveSrcsRaw) = unzip (map (\(s,i) -> makesrc i s) (zip srcs [(r+1)..]))
  			moveSrcs = concat moveSrcsRaw
  		in (moveSrcs ++ [LCall rdest func rsrcs,
  				LStore (makePtr dest) rdest
  				]
  				,r
  			)
  	(LLoad dest@(LTemp n t) src1) ->
  		let
  			(rsrc1, movesrc1) = (makesrc r src1)
  			rdest = (scratch (r+1) (getType dest))
  		in
  			(movesrc1 ++ [LLoad rdest rsrc1,
  				LStore (makePtr dest) rdest
  				],
  				r+2
  			)
  	(LBitcast dest@(LTemp n t) src1 targetType) ->
  		let
  			(rsrc1, movesrc1) = (makesrc r src1)
  			rdest = (scratch (r+1) (getType dest))
  		in
  			(movesrc1 ++ [LBitcast rdest rsrc1 targetType,
  				LStore (makePtr dest) rdest
  				],
  				r+2
  			)
  	(LStore dest src1) ->
  		let
  			(rsrc1, movesrc1) = (makesrc r src1)
  			rdest = (scratch (r+1) (getType dest))
  		in
  			(movesrc1 ++ [LStore rdest rsrc1,
  				LStore (makePtr dest) rdest
  				],
  				r+2
  			)
  	(LBrCond src1 iflabel elselabel) ->
  		let
  			(rsrc1, movesrc1) = (makesrc r (typifyLoc LBool src1))
  		in
  			(movesrc1 ++ [LBrCond rsrc1 iflabel elselabel],
  				r+1
  			)
  	(LBr label) -> ([LBr label],r)
  	(LMove dest@(LTemp n t) src1) ->
  		let
  			(rsrc1, movesrc1) = (makesrc r src1)
  			rdest = (scratch (r+1) (getType dest))
  		in
  			(movesrc1 ++ [LMove rdest rsrc1,
  				LStore (makePtr dest) rdest
  				],
  				r+2
  			)
  	(LRet src1) -> 
  		let
  			(rsrc1, movesrc1) = (makesrc r src1)
  		in
  			([LLoad rsrc1 (makePtr src1),
  				LRet rsrc1
  				],
  				r+1
  			)
  	LRetVoid -> ([LRetVoid],r)
  	(LStr str) -> ([LStr str],r)
  	LIf src1 ifbr elsebr -> 
  		let
  			(rsrc1, movesrc1) = (makesrc r src1)
  			(sifbr,r2) = stackifyInstrList (r+1) ifbr
  			(selsebr, r3) = stackifyInstrList (r2) elsebr
  		in
  			(movesrc1 ++ [LIf rsrc1 sifbr selsebr], r3)
  	LWhile src1 body ->
  		let
  			(rsrc1, movesrc1) = (makesrc r src1)
  			(sbody,r2) = stackifyInstrList (r+1) body
  		in
  			(movesrc1 ++ [LWhile rsrc1 (sbody ++ movesrc1)], r2)
  	(LLabel label) -> ([LLabel label],r)

makesrc r src@(LTemp n t) = 
	let
		rsrc = scratch r t
	in
		(rsrc, [LLoad rsrc src])
makesrc r src = (src,[])

scratch r ltype = LTemp (-r) ltype

getType loc = getLLocType loc

makePtr :: LLoc -> LLoc
makePtr (LTemp x t) = (LTemp x (LPtr t))
makePtr (LImm x t) = (LImm x (LPtr t))
makePtr (LHeap x t) = (LHeap x (LPtr t))

isTemp (LTemp _ _) = True
isTemp _ = False

hasType (LTemp _ LReserved) = False
hasType (LTemp _ _) = True

isReserved LReserved = True
isReserved _ = False
