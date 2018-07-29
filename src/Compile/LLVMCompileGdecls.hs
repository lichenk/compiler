module Compile.LLVMCompileGdecls(compileAllGdeclsLLVM) where
import Compile.Types
import Compile.Types.LLVM
import Compile.LLVMStructGen
import Compile.JankyLLVMSSA
import Compile.Parser
import Compile.Lexer
import qualified Compile.LLVMCodeGen as Safe
import qualified Compile.LLVMUnsafeCodeGen as Unsafe
import Compile.TypeCheck
import Compile.ScopeCheck
import Compile.ReturnCheck
import Compile.Types
import Data.List
import Data.Maybe
import Debug.Trace
import Compile.MakeLLVMLabels
import qualified Compile.MapWrap as MapWrap
import qualified Data.Map as Map
import Data.List as List
import qualified Data.Set as Set
import Compile.LLVMStructGen
import Compile.Types.LLVM
import Compile.Constants
import Compile.LLVMStackVariables
import Compile.EastToTast
-- ScrambleMap is a map from scrambled function name to original function name
type ScrambleMap = Map.Map Ident Ident
-- FuncMap[functionName] gives a tuple of the function's return type and the arguments
type FuncMap = Map.Map Ident (CType, [(CType, Ident)]) -- All CTypes here are guaranteed to be basic
-- StructMap[structName] gives you the field list of the struct
type StructMap = Map.Map Ident (Map.Map Ident CType)

type StructUseMap = Map.Map Ident (Set.Set Ident)
type FdefSet = Set.Set Ident

type CheckState = (FuncMap, StructMap, ScrambleMap, StructUseMap, FdefSet)


type AsmType = LInstr

additionalFuncs = [LHeaderFdecl LInt32 divName [LInt32, LInt32],
  LHeaderFdecl LInt32 modName [LInt32, LInt32],
  -- LHeaderFdecl LInt32 salName [LInt32, LInt32],
  -- LHeaderFdecl LInt32 sarName [LInt32, LInt32],
  LHeaderFdecl (LPtr LInt8) "calloc" [LInt32, LInt32],
  LHeaderFdecl (LVoid) "abort" [],
  LHeaderFdecl (LInt32) "raise" [LInt32]
  ]

{-
makeHeaderFuncs l =
  let
    sdecls = 
-}
-- Compile all gdecls given to us
compileAllGdeclsLLVM :: [Gdecl] -> [Gdecl] -> Bool -> Bool -> CheckState -> [Gdecl] -> [String]
compileAllGdeclsLLVM headerF headerS noOpt isSafe typeState l =
  let
    headerFuncs = List.map makeHeaderFunc headerF
    sdefs = List.filter isSdef (l ++ headerS)
    fdefs = List.filter isFdef l
    structFunctionsOld = buildLLVMStructs sdefs
    (arrStructs ,arrLookupMap) = buildArrayStructs l
    structFunctions = mergeLookup structFunctionsOld arrLookupMap
    (finalAsm, _) = List.foldl (compileFdefs noOpt isSafe structFunctions typeState) ([],0) fdefs
    structllvm = (List.map (\sdef -> LStr (show (genLLVMStruct sdef))) sdefs) ++ (List.map (\sdef -> LStr $ show sdef) arrStructs)
  in
    (List.map (show) structllvm) ++ (map show headerFuncs) ++ (List.map (show) finalAsm) ++ (List.map show additionalFuncs)

makeHeaderFunc (Fdecl ctype ident paramlist) =
  LHeaderFdecl (convertType ctype) ident (List.map (\(Param t _) -> (convertType t)) paramlist)

-- Purpose:
-- The reason is that the label number of jumps must be distinct in one assembly file.
-- Thus we must increment the label number each time, and keep track of the last label number
-- we used when compiling the previous function.
compileFdefs :: Bool -> Bool -> LGenStructInfo -> CheckState -> ([LFdecl], LabelNum) -> Gdecl -> ([LFdecl], LabelNum)
compileFdefs noOpt isSafe structFunctions typeState (prevFuncs,prevLabelnum) fdef =
  let
    (newFunc, newLabelnum) = case isSafe of
      True -> compileFunction noOpt isSafe structFunctions typeState fdef prevLabelnum
      False -> compileFunctionUnsafe noOpt isSafe structFunctions typeState fdef prevLabelnum
  in
    (newFunc:prevFuncs, newLabelnum)


-- compileFunction functionName finalAst arglist labelnum, where
-- fdef: Function definition we are compiling now
-- labelnum: Label number that we should start at
-- Returns:
-- (newAsm, newLabelnum) where
-- newAsm is new list of assembly including the function we just compiled
-- newLabelnum: New label number to start the next function at
compileFunctionUnsafe :: Bool -> Bool -> LGenStructInfo -> CheckState -> Gdecl -> LabelNum -> (LFdecl, LabelNum)
compileFunctionUnsafe noOpt isSafe structFunctions typeState fdef@(Fdefn functionType functionName paramlist block) labelnum =
  let
    truncatedFdef = checkAndTruncateReturn fdef
    (funcmap,_,_,_,_) = typeState
    newfuncmap = Map.map (\(t,_) -> Unsafe.fuckType $ convertType t) funcmap
    fields = Unsafe.astParamsToLLVMParams paramlist
    body = Unsafe.llvmcodeGen structFunctions newfuncmap truncatedFdef
    llvmFunc = LFdecl (Unsafe.ctypeToLtype functionType) functionName fields body
    -- ssa = llvmSSA llvmFunc
    (newlabel, labeled) = makeLabels labelnum llvmFunc
    stackified = stackifyFdecl labeled
  in
    (stackified, newlabel)
    -- trace (unlines (map show body)) $ (stackified, newlabel)


-- compileFunction functionName finalAst arglist labelnum, where
-- fdef: Function definition we are compiling now
-- labelnum: Label number that we should start at
-- Returns:
-- (newAsm, newLabelnum) where
-- newAsm is new list of assembly including the function we just compiled
-- newLabelnum: New label number to start the next function at
compileFunction :: Bool -> Bool -> LGenStructInfo -> CheckState -> Gdecl -> LabelNum -> (LFdecl, LabelNum)
compileFunction noOpt isSafe structFunctions typeState fdef@(Fdefn functionType functionName paramlist block) labelnum =
  let
    truncatedFdef = checkAndTruncateReturn fdef
    (funcmap,_,_,_,_) = typeState
    newfuncmap = Map.map (\(t,_) -> Safe.fuckType $ convertType t) funcmap
    fields = Safe.astParamsToLLVMParams paramlist
    body = Safe.llvmcodeGen structFunctions newfuncmap truncatedFdef
    llvmFunc = LFdecl (Safe.ctypeToLtype functionType) functionName fields body
    -- ssa = llvmSSA llvmFunc
    (newlabel, labeled) = makeLabels labelnum llvmFunc
    stackified = stackifyFdecl labeled
  in
    (stackified, newlabel)
    -- trace (unlines (map show body)) $ (stackified, newlabel)

isSdef :: Gdecl -> Bool
isSdef (Sdef _ _) = True
isSdef _ = False

isFdef :: Gdecl -> Bool
isFdef (Fdefn _ _ _ _) = True
isFdef _ = False 

