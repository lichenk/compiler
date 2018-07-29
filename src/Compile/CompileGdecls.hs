module Compile.CompileGdecls(compileAllGdecls) where
import Compile.Types
import Compile.Types.LLVM
import Compile.LLVMStructGen
import Compile.JankyLLVMSSA
import Compile.MakeLLVMLabels
import Compile.Parser
import Compile.Lexer
import Compile.CodeGen
import Compile.IntermediaryASMConvert
import Compile.NewInstructionSelection
import Compile.RegisterDistribution
import Compile.LivenessAnalysis
import Compile.RegisterAllocation
import Compile.TypeCheck
import Compile.ScopeCheck
import Compile.Dataflow
import Compile.ReturnCheck
import Compile.LoopCount
import Compile.LivenessAnalysis
import Compile.Types
import Compile.GenerateStruct
import Data.List
import Data.Maybe
import Debug.Trace
import Compile.MakeSSA
import Compile.Types.JumpAbstractAssembly
import Compile.ProcessLabels
import Compile.JumpGen
import Compile.JasmToIasm
import Compile.Propagation
import Compile.ParameterizeJAsm
import Compile.SSAJasm
import Compile.DeSSA
import Compile.CleanAssembly
import Compile.SafeCodeGen (safeCodeGen)
import Compile.LLVMStackVariables
import qualified Compile.JDataflow as JFlow
import qualified Compile.JLivenessAnalysis as JLive
import qualified Compile.MapWrap as MapWrap
import qualified Data.Map as Map
import Data.List as List
import Data.Set as Set

type AsmType = RAsm

-- Compile all gdecls given to us
compileAllGdecls :: Bool -> Bool -> [Gdecl] -> [AsmType]
compileAllGdecls noOpt isSafe l =
  let
    sdefs = List.filter isSdef l
    fdefs = List.filter isFdef l
    structFunctions = buildStructs sdefs
    (finalAsm, _) = List.foldl (compileFdefs noOpt isSafe structFunctions) ([],0) fdefs
  in
  	List.concat finalAsm

-- Purpose:
-- The reason is that the label number of jumps must be distinct in one assembly file.
-- Thus we must increment the label number each time, and keep track of the last label number
-- we used when compiling the previous function.
compileFdefs :: Bool -> Bool -> GenStructInfo -> ([[AsmType]], LabelNum) -> Gdecl -> ([[AsmType]], LabelNum)
compileFdefs noOpt isSafe structFunctions (prevRasm,prevLabelnum) fdef =
	let
		(newRasm, newLabelnum) = compileFunction noOpt isSafe structFunctions fdef prevLabelnum
	in
		(newRasm:prevRasm, newLabelnum)


-- compileFunction functionName finalAst arglist labelnum, where
-- fdef: Function definition we are compiling now
-- labelnum: Label number that we should start at
-- Returns:
-- (newAsm, newLabelnum) where
-- newAsm is new list of assembly including the function we just compiled
-- newLabelnum: New label number to start the next function at
compileFunction :: Bool -> Bool -> GenStructInfo -> Gdecl -> LabelNum -> ([AsmType], LabelNum)
compileFunction noOpt isSafe structFunctions fdef@(Fdefn functionType functionName paramlist block) labelnum =
  let
    truncatedFdef = checkAndTruncateReturn fdef
    abstractAsm = (if isSafe then safeCodeGen else codeGen) structFunctions truncatedFdef
    ssa = makeSSA abstractAsm
    (jumpGenedAsm, labelnum1dot5) = convertToJmp abstractAsm labelnum
    jasm = preprocessJAsm jumpGenedAsm
    jflow = JFlow.findControlFlow jasm
    jlive = JLive.findLiveSets jflow
    jparam = parameterize jlive
    ssaJasm = makeSSAJasm jparam
    thing = ssaJasm
    propJasm = copyConstantProp thing
    (dessa,labelnum2Opt) = deSSA labelnum1dot5 propJasm
    iAsmOpt = convertJasmToIasm dessa
    (iAsmUnopt, labelnum2Unopt) = convertTo2AddressFrom3Address labelnum ssa
    (newIAsm, labelnum2) = (iAsmUnopt, labelnum2Unopt) -- if noOpt then (iAsmUnopt, labelnum2Unopt) else (iAsmOpt, labelnum2Opt)
    progFlow = findControlFlow newIAsm
    analyzedProgram = findLiveSets newIAsm progFlow
    (liveSets, _, _) = unzip3 analyzedProgram
    loopscore = if noOpt then Map.map (\v -> 0) (scoreVariables newIAsm) else scoreVariables newIAsm
    -- loopscore = Map.map (\v -> 0) (scoreVariables newIAsm)
    registerColors = allocateRegisters progFlow analyzedProgram newIAsm loopscore
    allocedRegisters = distributeRegisters registerColors
    --generateAssembly :: LabelNum -> RegisterMapping -> [(IAsm, Set.Set ILoc)] -> Ident -> ([AsmType], LabelNum)
    (assembly, labelnum3) = generateAssembly labelnum2 allocedRegisters (zip newIAsm liveSets) functionName
    cleanAssembly = assembly -- if noOpt then assembly else cleanProgram assembly
  in
      (cleanAssembly, labelnum3)

isSdef :: Gdecl -> Bool
isSdef (Sdef _ _) = True
isSdef _ = False

isFdef :: Gdecl -> Bool
isFdef (Fdefn _ _ _ _) = True
isFdef _ = False 

