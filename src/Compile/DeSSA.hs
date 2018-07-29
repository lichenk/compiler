module Compile.DeSSA where

import qualified Data.Map as Map
import qualified Compile.MapWrap as MapWrap
import qualified Data.Set as Set
import Compile.Types
import Compile.Types.JumpAbstractAssembly

type BijectionMap = Map.Map JLoc JLoc
type LabelMap = Map.Map Int [JLoc]
type Info = (BijectionMap, LabelMap, Int, Int)

deSSA :: Int -> [JAsm] -> ([JAsm],Int)
deSSA labelnum jasmList = 
	let
		bijmap = makeNxNtoNBijectionMap jasmList
		labelmap = getLabelTable jasmList
		info = (bijmap, labelmap, labelnum, length (Map.keys bijmap))
		((_,_,newlabelnum,_),mainJasm,phiFunctionJasm) = deSSAList info jasmList
	in
		(mainJasm ++ phiFunctionJasm, newlabelnum)

deSSAList :: Info -> [JAsm] -> (Info, [JAsm],[JAsm])
deSSAList info (jasm:jasmList) =
	let
		(newInfo, mainJasm, phiFunctionJasm) = deSSALine info jasm
		(finalInfo, finalMainJasm, finalPhiFunctionJasm) = deSSAList newInfo jasmList
	in
		(finalInfo, mainJasm ++ finalMainJasm, phiFunctionJasm ++ finalPhiFunctionJasm)
deSSAList info [] = (info, [], [])

deSSALine :: Info -> JAsm -> (Info, [JAsm], [JAsm])
deSSALine info (JAsm dest op src) =
	(info, [JAsm (convertLocs info dest) op (convertVals info src)], [])
deSSALine info (JRet src) =
	(info, [JRet (convertVal info src)], [])
deSSALine info (JLabel label ssaArgs) =
	(info, [JLabel label (convertLocs info ssaArgs)], [])
deSSALine info jasm@(JJmp label ssaArgs jumpType) =
	deSSAJump info jasm
deSSALine info (JCall dest func args) =
	(info, [JCall (convertLoc info dest) func (convertVals info args)], [])

isDistinct :: (JVal, JLoc) -> Bool
isDistinct t@(val, loc) = not(isEqual t)

isEqual (JLoc loc1, loc2) = (loc1 == loc2)
isEqual _ = False

deSSAJump :: Info -> JAsm -> (Info, [JAsm], [JAsm])
deSSAJump info@(bijMap,labelmap,new_label_num, tempnum) (JJmp label oldssaArgs jumpType) =
	let
		ssaArgs :: [JVal]
		ssaArgs = convertVals info oldssaArgs
		argDests :: [JLoc]
		argDests = convertLocs info (labelmap Map.! label)
		argSrcDestTuple::[(JVal,JLoc)]
		argSrcDestTuple = zip ssaArgs argDests
		argSrcDestTupleNoSelfMoves::[(JVal,JLoc)]
		argSrcDestTupleNoSelfMoves = filter isDistinct argSrcDestTuple
		dests::[JLoc]
		srcs::[JVal]
		(srcs, dests) = unzip argSrcDestTupleNoSelfMoves
		mayOverwrite :: Set.Set JLoc
		mayOverwrite = Set.intersection (Set.fromList (valToLocs srcs)) (Set.fromList dests)
		mayOverwriteList :: [JLoc]
		mayOverwriteList = Set.toList mayOverwrite
		saveCopy :: [(JLoc, JLoc)]
		saveCopy = zip mayOverwriteList (map intToLoc [tempnum..])
		saveCopyInstr :: [JAsm]
		saveCopyInstr = map makeMoveLocs saveCopy
		saveCopyMap :: Map.Map JLoc JLoc
		saveCopyMap = Map.fromList saveCopy
		newsrcs :: [JVal]
		newsrcs = map (replace saveCopyMap) srcs
		argMovesPartial :: [JAsm]
		argMovesPartial = map makeMove (zip newsrcs dests) 
		argMoves:: [JAsm]
		argMoves = saveCopyInstr ++ argMovesPartial
		newJump = [JJmp label ssaArgs jumpType]
		newTempNum = tempnum + (length mayOverwriteList)
	in
		case ((length argMoves) == 0, jumpType) of
			(True,_) -> (
				(bijMap,labelmap ,new_label_num,newTempNum),
				newJump,[])
			(False,RJump) ->
				((bijMap,labelmap ,new_label_num,newTempNum),
					argMoves ++ newJump, [])
			(False,_) ->
				((bijMap,labelmap ,new_label_num+1,newTempNum),
					[JJmp new_label_num ssaArgs jumpType],
					[JLabel new_label_num argDests] ++ argMoves ++ [JJmp label ssaArgs RJump]
					)
intToLoc :: Int -> JLoc
intToLoc n = JTemp n 0 CAny
replace :: (Map.Map JLoc JLoc) -> JVal -> JVal
replace m (JLoc loc) =
	case Map.member loc m of
		True ->  JLoc $ m Map.! loc
		False -> JLoc $ loc
replace m x = x

makeMoveLocs :: (JLoc,JLoc) -> JAsm
makeMoveLocs (src, dest) =
	JAsm [dest] ANop [JLoc src]

makeMove (src, dest) =
	JAsm [dest] ANop [src]

-- labelmap[n][loc] tells you the new loc to map to
getLabelTable :: [JAsm] -> LabelMap
getLabelTable jasm =
	let
		labels = filter isLabel jasm
		labelTable = map makeLabelEntry labels
		labelMap = Map.fromList labelTable
	in
		labelMap

valToLocs vals = map valToLoc (filter isLoc vals)
valToLoc (JLoc loc) = loc
isLoc (JLoc loc) = True
isLoc _ = False

locToVals locs = map locToVal locs
locToVal loc = JLoc loc

isLabel :: JAsm -> Bool
isLabel (JLabel label ssaArgs) = True
isLabel _ = False

makeLabelEntry :: JAsm -> (Int, [JLoc])
makeLabelEntry (JLabel label ssaArgs) =
	(label, ssaArgs)

convertVal :: Info -> JVal -> JVal
convertVal info (JLoc loc) =
	JLoc (convertLoc info loc)
convertVal _ (val@(JImm imm)) = val

convertLoc :: Info -> JLoc -> JLoc
convertLoc (bij,_,_,_) loc@(JTemp n g t) = bij Map.! loc
convertLoc info (JHeap val t) = JHeap (convertVal info val) t
convertLoc info loc = loc

convertVals info vals = map (convertVal info) vals
convertLocs info locs = map (convertLoc info) locs

makeNxNtoNBijectionMap :: [JAsm] -> BijectionMap
makeNxNtoNBijectionMap jasm =
	let
		allLocs = getAllLocs jasm
		-- Filter temps
		srcTempMapList = Set.toList (Set.fromList (filter isTemp allLocs))
		targTempMapList = map makeNewTemp (zip srcTempMapList [0..])
		bijMapList = zip srcTempMapList targTempMapList
	in
		Map.fromList bijMapList

getAllLocs jasm = concat (map getLocs jasm)

getLocs :: JAsm -> [JLoc]
getLocs (JAsm dest op src) = dest
getLocs (JRet src) = []
getLocs (JLabel label ssaArgs) = ssaArgs
getLocs (JJmp label ssaArgs jumpType) = []
getLocs (JCall dest func args) = [dest]


makeNewTemp :: (JLoc,Int) -> JLoc
makeNewTemp (loc,n) =
	JTemp n 0 (getLocType loc)

isTemp :: JLoc -> Bool
isTemp (JTemp _ _ _) = True
isTemp  _ = False


getLocType :: JLoc -> CType
getLocType (JReg _ _ t) = t
getLocType (JTemp _ _ t) = t
getLocType (JMem _ _ t) = t
getLocType (JInArg _ _ t) = t
getLocType (JArg _ _ t) = t
getLocType (JHeap _ t) = t
