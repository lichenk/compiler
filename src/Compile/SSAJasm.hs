module Compile.SSAJasm where
import qualified Data.Map as Map
import qualified Compile.MapWrap as MapWrap
import Compile.Types
import Compile.Types.JumpAbstractAssembly
data RawJLoc = RawJReg Int
						 | RawJTemp Int
						 | RawJMem Int
						 | RawJInArg Int
						 | RawJArg Int
						 | RawJHeap deriving (Ord,Eq,Show)
type GenerationMap = Map.Map RawJLoc Int

makeSSAJasm :: [JAsm] -> [JAsm]
makeSSAJasm jasm =
	ssaJasmList Map.empty jasm

ssaJasmList :: GenerationMap -> [JAsm] -> [JAsm]
ssaJasmList genMap (jasm:jasmList) =
	let
		(newMap,newJasm) = (ssaLine genMap jasm)
	in
  	newJasm:(ssaJasmList newMap jasmList)
ssaJasmList genMap [] = []

ssaLine :: GenerationMap -> JAsm -> (GenerationMap,JAsm)
ssaLine genMap (JAsm dest op val) =
	let
		newGenMap = incrementGenerationList genMap dest
	in
		(newGenMap,
			JAsm
			(updateLocs newGenMap dest) -- Use newGenMap for dest
			op
			(updateVals genMap val)) --Use old genMap for source operands
ssaLine genMap (JRet val) =
	-- Nothing to update, since nothing is defined
	(genMap, JRet (updateVal genMap val))
ssaLine genMap (JLabel label ssaArgs) =
	let
		-- ssaArgs in a label are considered to be defined
		newGenMap = incrementGenerationList genMap ssaArgs
	in
		(newGenMap,
			JLabel
			label
			(updateLocs newGenMap ssaArgs) --Use newGenMap here!!!
			)
ssaLine genMap (JJmp label ssaArgs jumpType) =
	-- Do NOT increment ssaArgs!! This is because JJmp is USING the args, not DEFINING them
	(genMap, JJmp label (updateVals genMap ssaArgs) jumpType)
ssaLine genMap (JCall dest func args) =
	let
		newGenMap = incrementGeneration genMap dest
	in
		(newGenMap,
			JCall
			(updateLoc newGenMap dest)
			func
			(updateVals genMap args) -- Use OLD gen map for source operands
			)

incrementGenerationList :: GenerationMap -> [JLoc] -> GenerationMap
incrementGenerationList genMap varList =
	foldl incrementGeneration genMap varList

incrementGeneration :: GenerationMap -> JLoc -> GenerationMap
incrementGeneration genMap var =
	let
		rawvar = makeRawJLoc var
	in
	case (var, Map.member rawvar genMap) of
		(JHeap _ _,_)  -> genMap -- Don't store generations for heap
		(_,True) -> Map.insert rawvar (genMap Map.! rawvar + 1)  genMap
		(_,False) -> Map.insert rawvar 0  genMap -- Initialize to generation 0

updateVals :: GenerationMap -> [JVal] -> [JVal]
updateVals generationMap vals = map (updateVal generationMap) vals
updateVal :: GenerationMap -> JVal -> JVal
updateVal genMap (JImm imm) = (JImm imm) --Immediates have no generation number
updateVal genMap (JLoc loc) =
	JLoc (updateLoc genMap loc)
updateLocs :: GenerationMap -> [JLoc] -> [JLoc]
updateLocs generationMap locs = map (updateLoc generationMap) locs
updateLoc :: GenerationMap -> JLoc -> JLoc
updateLoc genMap loc@(JReg n g t) = JReg n (lookupGen genMap loc) t
updateLoc genMap loc@(JTemp n g t) = JTemp n (lookupGen genMap loc) t
updateLoc genMap loc@(JMem n g t) = JMem n (lookupGen genMap loc) t
updateLoc genMap loc@(JInArg n g t) = JInArg n (lookupGen genMap loc) t
updateLoc genMap loc@(JArg n g t) = JArg n (lookupGen genMap loc) t
updateLoc genMap (JHeap val t) = JHeap (updateVal genMap val) t

lookupGen genMap inloc =
	let
		loc = makeRawJLoc inloc
	in
	case (Map.member loc genMap) of
		True -> genMap Map.! loc
		False -> 0


makeRawJLoc (JReg n _ _  ) = RawJReg n
makeRawJLoc (JTemp n _ _ ) = RawJTemp n
makeRawJLoc (JMem n _ _  ) = RawJMem n
makeRawJLoc (JInArg n _ _) = RawJInArg n
makeRawJLoc (JArg n _ _  ) = RawJArg n
makeRawJLoc (JHeap jval _) = RawJHeap

