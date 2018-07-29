module Compile.BackPropagate(backPropagate) where

import qualified Vector as V
import qualified Compile.LivenessAnalysis as L

type PropMap = Map.Map ILoc IVal
type Info = V.Vector (IAsm, PropMap, [Int], [Int])

backPropagateAll :: Info
backPropagateAll info =
	let
		commitmap = foldl info (Map.empty, 0) [0..(V.length info)]
		(instrList,_,_,_) = Vector.unzip4	info
		instrTupleList = map (\instr -> (instr,Nothing)) instrList
		commitmapList = Map.toList commitmap
		instrCommitTupleList = map 



backPropagate :: 
backPropagate info (commitmap, lastLabel) linenum =
	let
		(instr, prop, succList, predList) = info (V.!) linenum
		rules = L.findUsedAndDefined instr
		usedVars = map L.getLoc (filter L.isUse rules)
		useConflict = filter (isVarConflict prop) usedVars
	in
		case instr of
			(IAsmlabel _)	-> (commitmap, linenum)
			_ -> foldl (insertToCommitMap lastLabel) commitmap useConflict

insertToCommitMap lastLabel commitmap var =
	Map.insert lastLabel var commitmap

isVarConflict prop var =
	case prop (Map.!) var of
		Conflict -> True
		_ -> False
