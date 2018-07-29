{- 
    Generates Assembly for each Function
-}

module Compile.CompileFunction (compileAllFunctions) where



import Compile.Types
import Compile.Parser
import Compile.Lexer
import Compile.CodeGen
import Compile.IntermediaryASMConvert
import Compile.MakeSSA (makeSSA)
import Compile.NewInstructionSelection
import Compile.RegisterDistribution
import Compile.LivenessAnalysis
import Compile.RegisterAllocation
import Compile.TypeCheck
import Compile.ScopeCheck
import Compile.Dataflow
import Compile.ReturnCheck
import Compile.LoopCount
import Compile.GlobalTypeFind
import Compile.LivenessAnalysis
import Compile.Types
import Data.List
import Data.Maybe
import Debug.Trace
import qualified Compile.MapWrap as MapWrap

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Compile.Types.EAST as E
import qualified Compile.Types.FAST as F

type FuncMap = Map.Map F.Ident ([(F.CType, F.Ident)], Maybe E.AST , F.CType) -- All F.CTypes here are guaranteed to be basic


-- Compiles all functions and returns the assembly list
compileAllFunctions :: FuncMap -> [RAsm]
compileAllFunctions functionDefinitionMap =
    let
    functionDefinitionList = Map.toList functionDefinitionMap
    (asm, labelnum) = foldl compileFunctionDefinition ([],0) functionDefinitionList
    in
    asm

-- compileFunctionDefinition (asm, labelnum) (functionName, (arglist, maybeAst , functionType)) where
-- asm: Assembly from previous functions
-- labelnum: Label number that we should start at
-- functionName: Name of function we are compiling
-- arglist: Argument list of function we are compiling
-- maybeAst: AST of function
-- functionType: Type of function
-- Returns:
-- (newAsm, newLabelnum) where
-- newAsm is new list of assembly including the function we just compiled
-- newLabelnum: New label number to start the next function at
-- Purpose: (AKA Why the fuck is the type of this function so complicated?)
-- The reason is that the label number of jumps must be distinct in one assembly file.
-- Thus we must increment the label number each time, and keep track of the last label number
-- we used when compiling the previous function.
compileFunctionDefinition :: ([RAsm], LabelNum) -> (F.Ident, ([(F.CType, F.Ident)], Maybe E.AST , F.CType)) -> ([RAsm], LabelNum)
compileFunctionDefinition (asm, labelnum) (functionName, (arglist, maybeAst , functionType)) =
    case maybeAst of
        Just ast -> (
            let
                (asmFunction, labelnum2) = compileFunction functionType functionName ast (map (\(ctype,ident) -> ident) arglist) labelnum
            in
                (asmFunction ++ asm, labelnum2)
            )
        Nothing -> (asm, labelnum)


-- compileFunction functionName finalAst arglist labelnum, where
-- functionName: Name of function we are compiling
-- finalAst: AST of function
-- arglist: Argument list of function we are compiling
-- functionType: Type of function
-- labelnum: Label number that we should start at
-- Returns:
-- (newAsm, newLabelnum) where
-- newAsm is new list of assembly including the function we just compiled
-- newLabelnum: New label number to start the next function at
-- Purpose: (AKA Why the fuck is the type of this function so complicated?)
-- The reason is that the label number of jumps must be distinct in one assembly file.
-- Thus we must increment the label number each time, and keep track of the last label number
-- we used when compiling the previous function.
compileFunction :: F.CType -> F.Ident -> E.AST -> [F.Ident] -> LabelNum -> ([RAsm], LabelNum)
compileFunction functionType functionName inAst arglist labelnum =
    -- By now the AST should be done, and we start with the assembly
    let
    truncatedAsm = checkAndTruncateReturn inAst functionType
    aAsm = codeGen truncatedAsm arglist
    ssa = makeSSA aAsm
    loopscore = scoreVariables ssa
    (iAsm, labelnum2) = convertTo2AddressFrom3Address labelnum $ ssa
    progFlow = findControlFlow iAsm
    analyzedProgram = findLiveSets iAsm progFlow
    (liveSets, _, _) = unzip3 analyzedProgram
    registerColors = allocateRegisters progFlow analyzedProgram iAsm loopscore
    allocedRegisters = distributeRegisters registerColors
    --generateAssembly :: LabelNum -> RegisterMapping -> [(IAsm, Set.Set ILoc)] -> Ident -> ([RAsm], LabelNum)
    (assembly, labelnum3) = generateAssembly labelnum2 allocedRegisters (zip iAsm liveSets) functionName
    in
    (assembly, labelnum3)

