{- 
    Generates Assembly
-}

module Compile.InstructionSelection (generateAssembly) where

import Compile.Types
import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Compile.MapWrap as MapWrap
import Compile.FunctionPrefix


-- Follow this list when using IReg and AReg!!
registers :: [String]
registers = [
    "eax", -- 0
    "ebx", -- 1
    "ecx", -- 2
    "edx", -- 3
    "esi", -- 4
    "edi", -- 5
    "ebp", -- 6
    "r8d",  -- 7
    "r9d",  -- 8
    "r10d", -- 9
    "r11d", -- 10
    "r12d", -- 11
    "r13d", -- 12
    "r14d", -- 13
    "r15d"  -- 14
    ]

argmap = [
    (RReg "edi"),
    (RReg "esi"),
    (RReg "edx"),
    (RReg "ecx"),
    (RReg "r8d"),
    (RReg "r9d")]

callerSaveRegs = [
    (RReg "rdi"),
    (RReg "rsi"),
    (RReg "rdx"),
    (RReg "rcx"),
    (RReg "r8"),
    (RReg "r9"),
    (RReg "r10"),
    (RReg "r11")]

mapIndex :: (a -> Int -> b) -> [a] -> [b]
mapIndex f l = zipWith f l [0..]


pushCallerSaveDumb = List.map (\reg -> RAsmUnary RPushq reg) callerSaveRegs
popCallerSaveDumb = List.map (\reg -> RAsmUnary RPopq reg) (List.reverse callerSaveRegs)

calleeSaveRegs = [
    (RReg "rbx"),
    (RReg "rbp"),
    (RReg "r12"),
    (RReg "r13"),
    (RReg "r14"),
    (RReg "r15")]
pushCalleeSave = List.map (\reg -> RAsmUnary (RPushq) (reg)) calleeSaveRegs
-- Popping must be done in REVERSE!
popCalleeSave = List.map (\reg -> RAsmUnary (RPopq) (reg)) (reverse calleeSaveRegs)

-- Find how much space we need to build the argument build area
-- that our function will use to call other functions
findMaxArgs :: [IAsm] -> Int
findMaxArgs iasmList = (foldl max 0 (map findNumArgs iasmList)) - (length argmap)

type RegisterMappingInfo = (RegisterMapping, Int)

generateAssembly :: LabelNum -> RegisterMapping -> [IAsm] -> Ident -> ([RAsm], LabelNum)
generateAssembly n rmap l functionName =
    let
        stackSize = (Set.size (Set.fromList (Map.elems rmap))) + 1
        -- Save callee-save registers and sub rsp
        subRsp = stackSize
        preStackText = 
            pushCalleeSave ++ [RAsmText ("\t" ++ (show RSubq) ++ "\t$" ++ (show (subRsp * 8)) ++ ", %rsp")]
        postStackText = 
            [RAsmText ("\t" ++ (show RAddq) ++ "\t$" ++ (show (subRsp * 8)) ++ ", %rsp")] ++ popCalleeSave
        cleanUp :: [RAsm] -> [RAsm]
        cleanUp [] = []
        cleanUp (RRet:xs) = postStackText ++ [RRet] ++ (cleanUp xs)
        cleanUp ((RAsm dst RMove src):xs) =
            case (dst, src) of
                (RReg x,  RLoc (RReg y)) -> if x == y then (cleanUp xs) else (RAsm dst RMove src):(cleanUp xs)
                (RMem x,  RLoc (RMem y)) -> if x == y then (cleanUp xs) else (RAsm dst RMove src):(cleanUp xs)
                otherwise -> (RAsm dst RMove src):(cleanUp xs)
        cleanUp (x:xs) = x:(cleanUp xs)
        (asm, labelnum) = selectInstructions (rmap, parentStackOffset) l n

        --cleanRet l = List.take (length l - (Maybe.fromJust $ List.elemIndex RRet $ reverse l) - 1) l
    in
        ((asmHeader functionName) ++ preStackText ++ (cleanUp asm), labelnum)

asmHeader :: Ident -> [RAsm]
asmHeader functionName = [RAsmText ("\t.globl\t" ++ functionName),
                RAsmText "",
                RAsmText (functionName ++ ":")]

signHack :: [RAsm]
signHack = [RAsm (RReg "rax") RSalq (RImm 32), RAsm (RReg "rax") RSarq (RImm 32)]


selectInstructions :: RegisterMappingInfo -> [IAsm] -> LabelNum -> ([RAsm], LabelNum)
selectInstructions rmapinfo [] n = ([], n)
selectInstructions rmapinfo (x:xs) n =
    let
        (asm, num) = convertSingleExp rmapinfo x n
        (asm2, num2) = selectInstructions rmapinfo xs num
    in
        (asm ++ asm2, num2)

convertSingleExp :: RegisterMappingInfo -> IAsm -> LabelNum -> ([RAsm], LabelNum)
-- Convert instruction
convertSingleExp rmapinfo (IAsm [dest] op src) num =
    let
        nDest = (convertLoc dest rmapinfo)
        cSource = (convertVal src rmapinfo)
        (pre, nSource) = checkIfBothMemLoc nDest cSource
        newOp = (convertOp op)
        
        isDestMemoryLocation :: RLoc -> Bool
        isDestMemoryLocation (RMem _) = True
        isDestMemoryLocation _ = False

        (body, nnum) = if getBinopType op == CBool
                then
                    convertConditional rmapinfo nDest nSource op num
                else 
                    (case newOp of
                    RDiv -> [RAsm (RReg "eax") RMove (RLoc nDest), RAsm (RReg "edx") RMove (RLoc nDest), RAsm (RReg "edx") RSarl (RImm 31), RAsmText ("\t" ++ (show newOp) ++ "\t" ++ (show nSource)), RAsm nDest RMove (RLoc (RReg "eax"))]
                    RMod -> [RAsm (RReg "eax") RMove (RLoc nDest), RAsm (RReg "edx") RMove (RLoc nDest), RAsm (RReg "edx") RSarl (RImm 31), RAsmText ("\t" ++ (show newOp) ++ "\t" ++ (show nSource)), RAsm nDest RMove (RLoc (RReg "edx"))]
                    RSarl -> [RAsm (RReg "ecx") RMove (nSource), RAsm (nDest) newOp (RLoc $ RReg "cl")]
                    RSall -> [RAsm (RReg "ecx") RMove (nSource), RAsm (nDest) newOp (RLoc $ RReg "cl")]
                    RMove -> [RAsm (nDest) newOp (nSource)]
                    RMul -> if (isDestMemoryLocation nDest) then [RAsm (convertSrcIntoDest nSource) RMul (RLoc nDest), RAsm nDest RMove nSource] else [RAsm (nDest) newOp (nSource)]
                    _ -> [RAsm (nDest) newOp (nSource)]
                    , num)
    in
        (pre ++ body, nnum)
-- Error
convertSingleExp rmapinfo (IAsm _ op src) _ = 
    error "InstructionSelection: Too many destinations"
-- Return instruction
convertSingleExp rmapinfo (IRet retval) n = ([RRet], n)
-- Label
convertSingleExp rmapinfo (IAsmLabel str) n = ([RAsmLabel str], n)
-- Jump
convertSingleExp rmapinfo (IAsmJump t str) n = ([RAsmJump t str], n)

convertSingleExp (rmap, stackVarOffset, saveOffset, parentStackOffset) (ISave _) n = (pushCallerSave saveOffset, n)
convertSingleExp (rmap, stackVarOffset, saveOffset, parentStackOffset) (IRestore _) n = (popCallerSave saveOffset, n)
convertSingleExp (rmap, stackVarOffset, saveOffset, parentStackOffset) (ICall functionName numArgs) n =
    (pushCallerSaveDumb ++ (setupCall ) ++ [RCall (functionName)] ++ popCallerSaveDumb, n)
-- JUST ABOVE THIS
convertConditional :: RegisterMappingInfo -> RLoc -> RVal -> AOp -> LabelNum -> ([RAsm], LabelNum)
convertConditional rmapinfo dest src op n =
    let
        conditional = [RAsm dest RCmp src]
        jumpToElse = [RAsmJump (getJumpOp op) (".L" ++ (show (n)))]
        jumpToEnd = [RAsmJump RJump (".L" ++ (show (n + 1)))]
        elseLabel = [RAsmLabel (".L" ++ (show (n)))]
        endLabel = [RAsmLabel (".L" ++ (show (n + 1)))]
        asm1 = [RAsm dest RMove (RImm 0)]
        asm2 = [RAsm dest RMove (RImm 1)]
    in
        (conditional ++ jumpToElse ++ asm1 ++ jumpToEnd ++ elseLabel ++ asm2 ++ endLabel, n + 2)

convertSrcIntoDest :: RVal -> RLoc
convertSrcIntoDest (RLoc x) = x
convertSrcIntoDest _ = error "InstructionSelection: Cannot convert an immediate into dest"

-- Can change the source
checkIfBothMemLoc :: RLoc -> RVal -> ([RAsm], RVal)
checkIfBothMemLoc (RMem _) (RLoc (RMem n)) =
    ([RAsm (RReg "r15d") RMove (RLoc (RMem n))], RLoc (RReg "r15d"))
checkIfBothMemLoc _ src = ([], src)

convertVal :: IVal -> RegisterMappingInfo -> RVal
convertVal (ILoc x) rmapinfo = (RLoc (convertLoc x rmapinfo))
convertVal (IImm n) rmapinfo = (RImm n)


convertLoc :: ILoc -> RegisterMappingInfo -> RLoc
-- There is a special case here, where IReg can only be eax
convertLoc (IReg n) rmapinfo = case (n < length registers) of
    True -> RReg (registers !! n)
    False -> error("IReg out of range" ++ show n)
convertLoc (ITemp n) (rmap, stackVarOffset, saveOffset, parentStackOffset) = 
    case rmap MapWrap.! n of
        RReg reg -> RReg reg
        RMem offset -> RMem (offset + stackVarOffset)
convertLoc (IMem n) rmapinfo = error "InstructionSelection: How is there a mem referance here?" -- (RMem n)
convertLoc (IArg n) rmapinfo = 
    case (n < (length argmap)) of
        True -> argmap !! n
        False -> RMem (n*8)

convertLoc (IInArg n)  (rmap, stackVarOffset, saveOffset, parentStackOffset) = 
    case (n < (length argmap)) of
        True -> argmap !! n
        False -> RMem ((n*8) + parentStackOffset)

convertOp :: AOp -> ROp
convertOp op = case op of   AAdd -> RAdd
                            AAddq -> RAddq
                            ASub -> RSub
                            ASubq -> RSubq
                            ADiv -> RDiv
                            AMul -> RMul
                            AMod -> RMod
                            ANop -> RMove
                            ABAnd -> RBAnd
                            ABXor -> RBXor
                            ABOr -> RBOr
                            AShiftL -> RSall
                            AShiftR -> RSarl
                            ACmp -> RCmp

getJumpOp :: AOp -> JumpType
getJumpOp op = case op of   ALess -> RJL
                            ALeq -> RJLe
                            AGeq -> RJGe
                            AGreater -> RJG
                            AEq -> RJe
                            ANeq -> RJne

getBinopType :: AOp -> CType
getBinopType binop =
    if elem binop [ALess, ALeq, AGeq, AGreater, AEq, ANeq]
    then CBool
    else CInt

