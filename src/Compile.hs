{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>
                Rokhini Prabhu <rokhinip@andrew.cmu.edu>

   Main compiler module; takes a job and compiles it
-}
module Compile
(compile
,Job(..)
,defaultJob
,OF(..)
) where

import System.FilePath
import System.Process
import System.Exit
import System.IO


import Compile.Types
import Compile.Parser
import Compile.Lexer
import Compile.IntermediaryASMConvert
-- import Compile.MakeSSA (makeSSA)
-- import Compile.RegisterDistribution
import Compile.LivenessAnalysis
-- import Compile.RegisterAllocation

import Compile.Dataflow

import Compile.LoopCount
-- import Compile.CompileFunction
-- import Compile.MangleFunctionName
import Compile.GenGDecls
import Compile.ScrambleFunctionName
import Compile.Types.FAST as F
import Compile.Types.TAST as T
import Compile.Types.EAST as E
import Compile.EastToTast
import Compile.SplitTokensByGdecl
import Compile.ParseGdeclTokenList
import Compile.ElaborateGdeclList
import Compile.CheckGdeclRules
import Compile.TypeCheckGdecl
import Compile.ScopeCheckGdecl
import Compile.Constants
import Compile.CompileGdecls
import Compile.UnLazifyTast
import Compile.UnfuckStruct
import Debug.Trace
import Data.List
import Data.Maybe
import Compile.Types.LLVM
import Compile.LLVMCompileGdecls


import qualified Data.Map as Map
import qualified Data.Set as Set

writer file obj = writeFile file obj

compileHeader header =
  let
    toks = lexProgram header
    gdeclTokenList = splitProgram [] toks
    (parseState, astList) = parseGdeclTokenList Set.empty gdeclTokenList
    -- Replace empty maps with things from header file!
    (elabState, elaborateAstList) = elaborateGdeclList (Map.empty, Map.empty) astList
    checkedAstList = if checkGdeclGood then elaborateAstList else error("gdecl check failed!")
    -- Check Gdecl list for any redefinitions, scoping of Gdecls
    (checkState, checkGdeclGood) = checkGdeclList (Set.empty, Map.empty, Set.empty, Set.empty, Map.empty, True, Map.empty) elaborateAstList

    -- Scopecheck
    (scopeState, scopeGood) = scopeGdeclListHeader Set.empty checkedAstList
    scopedAstList = if scopeGood then checkedAstList else error("Scope failed!")
    -- Typecheck
    (typeState, typedAstList) = typifyGdeclList (Map.empty, Map.empty, Map.empty, Map.empty, Set.empty) scopedAstList
    typestate2 = foldl buildfdefsetheader typeState scopedAstList
  in
    (parseState, elabState, checkState, scopeState, typestate2, scopedAstList)



-- compileDriver takes a raw string (the program's source) and 
-- the desired output format and outputs another
-- string, the compiled program.  Does no IO, so no monads!
compileDriver :: String -> String -> OF -> String -> Bool -> String
compileDriver source header format optstr isSafe =
  let
    -- Get states from header files
    (headerparseState, headerelabState, headercheckState, headerscopeState, headertypeState, headerscopedAstList) =
      compileHeader header
    -- Start parsing source files
    toks = lexProgram source
    gdeclTokenList = splitProgram [] toks
    (parseState, astListIn) = parseGdeclTokenList headerparseState gdeclTokenList
    astList = predeclaredFunctions ++ astListIn
    scrambleMap = getScrambleMap headerscopeState astList
    scrambledAst = scrambleAst headerscopeState astList
    -- Add predeclared functions
    astListWithPredeclaredFunctions = predeclaredFunctions ++ scrambledAst
    (elabState, elaborateAstList) = elaborateGdeclList headerelabState astListWithPredeclaredFunctions
    -- Check Gdecl list for any redefinitions, scoping of Gdecls
    newcheckState = insertScrambleMapIntoCheckStateAndMarkSource scrambleMap headercheckState
    (checkState, checkGdeclGood) = checkGdeclList newcheckState elaborateAstList
    checkedAstList = if checkGdeclGood then elaborateAstList else error("gdecl check failed!")
    -- Scopecheck
    (scopeState, scopeGood) = scopeGdeclListSource headerscopeState checkedAstList
    scopedAstList = if scopeGood then checkedAstList else error("Scope failed!")
    -- Typecheck
    -- Fuck function name shadowing. If it weren't for that, we won't need this scrambleMap shit
    newheadertypestate = insertScrambleMapIntoState headertypeState scrambleMap
    newtypestate1 = foldl buildfdefsetsrc newheadertypestate (predeclaredFunctionsE++scopedAstList)
    (typeState, typedAstList) = typifyGdeclList newtypestate1 scopedAstList
    -- Unfortunately, laziness forces us to do the following shit:
    -- Attempting to remove the following lines causes the following test cases to fail:
    -- ../tests0/mississippi-invalidfieldaccess.l4, ../tests0/newyork-teststatics10.l4
    doesOurShitTypeCheckStupidLazyHaskell = unlazifyTast typedAstList
    unlazyTypedAstList = case doesOurShitTypeCheckStupidLazyHaskell of
        True -> typedAstList
        False -> error("Typechecking failed!")
    -- Now unfuck structs
    unfuckedTypedAstList = unfuckTast unlazyTypedAstList
    -- asmList = genDecls typedAstList
    assembly = 
      case format of
        LLVM -> compileAllGdeclsLLVM (getTastFdecl headerscopedAstList) (getTastSdef headerscopedAstList) (optstr == "0") isSafe typeState unfuckedTypedAstList
        _ ->  map show $ compileAllGdecls (optstr == "0") isSafe unfuckedTypedAstList -- unlazyTypedAstList
  in
    unlines assembly


-- compile handles the IO for this thing.  It's nasty.
compile :: Job -> IO ()
compile job = 
    do
    handle <- openFile (jobSource job) ReadMode
    blah <- hSetEncoding handle utf8
    s <- hGetContents handle
    headerString <- case (jobHeader job) of
      Nothing -> return ""
      Just h -> do
        headerHandle <- openFile h ReadMode
        blah2 <- hSetEncoding headerHandle utf8
        -- hGetContents handle
        hGetContents headerHandle
    writer (jobOut job) $ compileDriver s headerString (jobOutFormat job) (jobOptimize job) (jobSafe job)

-- compile handles the IO for this thing.  It's nasty.
compileDebug :: String -> String -> IO ()
compileDebug filename header = 
    do
    handle <- openFile filename ReadMode
    blah <- hSetEncoding handle utf8
    s <- hGetContents handle
    headerHandle <- openFile header ReadMode
    blah2 <- hSetEncoding headerHandle utf8
    -- hGetContents handle
    headerString <- hGetContents headerHandle
    writer "output" $ compileDriver s headerString Asm "0" True