{- 
    Generates Assembly
-}

module Compile.NewInstructionSelection (generateAssembly) where

import Compile.Types
import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Compile.MapWrap as MapWrap
import Compile.Constants
import Compile.RegisterList

-- (rmap, parentStackOffset, selfStackOffset, callerSaveOffset)
-- rmap is the map from Int (Temp Num) to RLoc (RReg or RMem), 
-- parentStackOffset is 1/8 the distance from rsp to the parent's stack frame (first in argument)
-- selfStackOffset is distance from rsp to temp 0
-- callerSaveOffset is distance from rsp to caller save registers that you've saved on the stack to prepare for a call
type StackState = (RegisterMapping, Int, Int, Int)


generateAssembly :: LabelNum -> RegisterMapping -> [(IAsm, Set.Set ILoc)] -> Ident -> ([RAsm], LabelNum)
generateAssembly n rmap l functionName =
  let
    -- When we're done setting up the stack, we'll be 8*stackSize bytes away from the callee-save registers
    -- That is, stackSize is the labelnumber of temps we have
    stackSize = countStackVars rmap
    -- Because we're pushing callee-saves, we don't need to worry about that here
    subRsp = stackSize
    -- Instructions to set up and destroy the stack frame
    myCalleeSaves = determineCalleeSaves rmap functionName
    preStackText = 
      (pushCalleeSave myCalleeSaves) ++ [RAsm (RReg "rsp") RSubq (RImm (subRsp * 8))]
    postStackText = 
      [RAsm (RReg "rsp") RAddq (RImm (subRsp * 8))] ++ (popCalleeSave myCalleeSaves)
    (asm, labelnum, stackState) = selectInstructions (rmap, 1 + (length myCalleeSaves) + stackSize, 0, 0) l n

    -- cleanUp eliminates self-moves in the generated assembly code
    cleanUp :: [RAsm] -> [RAsm]
    cleanUp [] = []
    cleanUp (RRet:xs) = postStackText ++ [RRet] ++ (cleanUp xs)
    cleanUp ((RAsm dst (RMove ctype) src):xs) =
      case (dst, src) of
        (RReg x,  RLoc (RReg y)) -> if x == y then (cleanUp xs) else (RAsm dst (RMove ctype) src):(cleanUp xs)
        (RMem x,  RLoc (RMem y)) -> if x == y then (cleanUp xs) else (RAsm dst (RMove ctype) src):(cleanUp xs)
        otherwise -> (RAsm dst (RMove ctype) src):(cleanUp xs)
    cleanUp (x:xs) = x:(cleanUp xs)

  in
    ((asmHeader functionName) ++ preStackText ++ (cleanUp asm), labelnum)

asmHeader :: Ident -> [RAsm]
asmHeader functionName = [RAsmText ("\t.globl\t" ++ headerPrefix ++ functionName),
                RAsmText "",
                RAsmText (headerPrefix ++ functionName ++ ":")]
selectInstructions :: StackState -> [(IAsm, Set.Set ILoc)] -> LabelNum -> ([RAsm], LabelNum, StackState)
selectInstructions rmapinfo [] labelnum = ([], labelnum, rmapinfo)
selectInstructions rmapinfo1 ((ISave, _):instruction_list) labelnum1 =
  let
    liveset = findRestoreLivesets instruction_list
    (asm2, labelnum2, rmapinfo2) = convertSingleExp rmapinfo1 (ISave, liveset) labelnum1
    (asm3, labelnum3, rmapinfo3) = selectInstructions rmapinfo2 instruction_list labelnum2
  in
    (asm2 ++ asm3, labelnum3, rmapinfo3)
selectInstructions rmapinfo1 (instruction:instruction_list) labelnum1 =
  let
    (asm2, labelnum2, rmapinfo2) = convertSingleExp rmapinfo1 instruction labelnum1
    (asm3, labelnum3, rmapinfo3) = selectInstructions rmapinfo2 instruction_list labelnum2
  in
    (asm2 ++ asm3, labelnum3, rmapinfo3)

-- I just pushed an argument!
-- Thus rsp is now further away from parent stack, self stack variables and callersave variables
iJustPushedArg :: Int -> StackState -> StackState
iJustPushedArg n (rmap, parentStackOffset, selfStackOffset, callerSaveOffset) =
  (rmap, parentStackOffset + n, selfStackOffset + n, callerSaveOffset + n)

-- I just popped an argument!
-- Thus rsp is now closer to parent stack, self stack variables and callersave variables
iJustPoppedCallerArg :: Int -> StackState -> StackState
iJustPoppedCallerArg n (rmap, parentStackOffset, selfStackOffset, callerSaveOffset) =
  (rmap, parentStackOffset - n, selfStackOffset - n, callerSaveOffset - n)


-- I just pushed an caller save!
-- Thus rsp is now further away from parent stack and self stack variables.
iJustPushedCallerSave :: Int -> StackState -> StackState
iJustPushedCallerSave n (rmap, parentStackOffset, selfStackOffset, callerSaveOffset) =
  (rmap, parentStackOffset + n, selfStackOffset + n, callerSaveOffset)

-- I just popped an caller save!
-- Thus rsp is now closer to parent stack and self stack variables.
iJustPoppedCallerSave :: Int -> StackState -> StackState
iJustPoppedCallerSave n (rmap, parentStackOffset, selfStackOffset, callerSaveOffset) =
  (rmap, parentStackOffset - n, selfStackOffset - n, callerSaveOffset)

-- I just added rsp by (n*8)  ! <-- this exclamation mark is NOT factorial
-- Thus rsp is now closer to parent stack and self stack variables.
iJustAddedRsp :: Int -> StackState -> StackState
iJustAddedRsp addRspDiv8 (rmap, parentStackOffset, selfStackOffset, callerSaveOffset) =
  (rmap, parentStackOffset - addRspDiv8, selfStackOffset - addRspDiv8, callerSaveOffset - addRspDiv8)

-- Given an RLoc
-- We either convert the register from 32-bit to 64-bit
-- DO NOT reindex the stack here. It is already done by the function below (called by convertLoc)
makeReg64 :: StackState -> RLoc -> RLoc
makeReg64 rmapinfo (RReg reg) = reg32toreg64 (RReg reg)
makeReg64 state (RMem n) = RMem n


-- We re-index the stack variable according to selfStackOffset
reindexTempVarForStack :: StackState -> RLoc -> RLoc
reindexTempVarForStack state (RReg reg) = RReg reg
reindexTempVarForStack (rmap, parentStackOffset, selfStackOffset, callerSaveOffset) (RMem n) =
  RMem (n + (8*selfStackOffset)) --Reindex to selfStackOffset

convertSingleExp :: StackState -> (IAsm, Set.Set ILoc) -> LabelNum -> ([RAsm], LabelNum, StackState)
-- Convert instruction
convertSingleExp state@(rmap, parentStackOffset, selfStackOffset, callerSaveOffset) ((IAlignStack numArgs), _) labelnum =
  let
    stackArgs = max (numArgs-6) 0 --6 is number of args put on registers, rest are put on stack
  in
  case (mod (parentStackOffset+stackArgs) 2) of
    -- We know parent stack is 0 mod 16
    -- So if our distance to the parent stack is even, and the number of future pushes (due to numArgs)
    -- is even,
    -- then our final rsp is also 0 mod 16.
    0 -> ([], labelnum, state)
    -- Else, we push a dummy 8-byte thing to force rsp to be 0 mod 16
    _ ->
      let
        newrmapinfo = iJustPushedArg 1 state
      in
        ([RAsmUnary RPushq (RImm 0)], labelnum, newrmapinfo)

convertSingleExp rmapinfo ((IAsm [IArg _ _] op src), _) labelnum =
  let 
    pushed_arg :: RVal
    (pushed_arg, prefixInstructions) = case src of
      ILoc (IHeap val ctype) -> convertHeapVal rmapinfo (RReg "r13d") src
      ILoc loc -> (RLoc (makeReg64 rmapinfo (convertLoc loc rmapinfo)),[])
      IImm constant -> (
        case (constant > (fromIntegral max_32bit_unsigned)) of
          True -> (RLoc $ RReg "r15", [RAsm (RReg "r15") (RMove CEightByte) (RImm constant)])
          False ->(RImm constant, [])
          )
    pushed_arg_64 = pushed_arg
    instructions = prefixInstructions ++ [RAsmUnary RPushq (pushed_arg)]
    newrmapinfo = iJustPushedArg 1 rmapinfo
  in
    (instructions, labelnum, newrmapinfo)

convertSingleExp (state@(rmap, parentStackOffset, selfStackOffset, callerSaveOffset)) (ICall functionName numArgs, _) labelnum =
  let
    -- Adjust stack back to callerSaveOffset so that IRestore can restore them!
    -- We add back the amount that we pushed when pushing arguments
    addRsp = callerSaveOffset
    newStackState = iJustAddedRsp addRsp state
  in
  -- Call the fucking function
  -- Then adjust the stack back to before argument pushing
  ([RCall functionName, RAsm (RReg "rsp") RAddq (RImm (addRsp*8))], labelnum, newStackState)

convertSingleExp state (ISave, live) labelnum =
  let
    savedRegs = determineCallerSaves state live
    numSaves = length savedRegs
    pushes = map (\reg -> (RAsmUnary RPushq (RLoc reg))) savedRegs
    newstate = iJustPushedCallerSave numSaves state
  in
    (pushes, labelnum, newstate)

convertSingleExp state (IRestore, live) labelnum =
  let
    savedRegs = reverse $ determineCallerSaves state live
    numSaves = length savedRegs
    pushes = map (\reg -> (RAsmUnary RPopq (RLoc reg))) savedRegs
    newstate = iJustPoppedCallerSave numSaves state
  in
    (pushes, labelnum, newstate)

convertSingleExp (rmapinfo@(rmap, parentStackOffset, selfStackOffset, callerSaveOffset)) ((IAsm [dest] op src), _) labelnum =
  let
    -- nDestMov is a list of assembly instructions in case we need
    -- to move the address of dest to a register to dereference it
    (nDest, nDestMov) = convertHeap rmapinfo (RReg "r14d") dest
    (cSource, nSourceMov) = convertHeapVal rmapinfo (RReg "r13d") src
    (pre, nSource) = checkIfNeedToMoveAndMove nDest cSource op
    newOp = convertOp op (getLocType dest)
    (body, newlabelnum) = 
      if getBinopType op == CBool
      then
        convertConditional rmapinfo nDest nSource op labelnum
      else 
        (case newOp of
        RDiv ctype -> [RAsm (RReg "eax") (RMove ctype) (RLoc nDest),
          RAsm (RReg "edx") (RMove ctype) (RLoc nDest),
          RAsm (RReg "edx") (RSar  ctype) (RImm 31),
          RAsmText ("\t" ++ (show newOp) ++ "\t" ++ (show nSource)),
          RAsm nDest (RMove ctype) (RLoc (RReg "eax"))]
        RMod ctype -> [RAsm (RReg "eax") (RMove ctype) (RLoc nDest),
          RAsm (RReg "edx") (RMove ctype) (RLoc nDest),
          RAsm (RReg "edx") (RSar  ctype) (RImm 31),
          RAsmText ("\t" ++ (show newOp) ++ "\t" ++ (show nSource)),
          RAsm nDest (RMove ctype) (RLoc (RReg "edx"))]
        RSar ctype -> [RAsm (RReg "ecx") (RMove ctype) (nSource),
          RAsm (nDest) newOp (RLoc $ RReg "cl")]
        RSal ctype -> [RAsm (RReg "ecx") (RMove ctype) (nSource),
          RAsm (nDest) newOp (RLoc $ RReg "cl")]
        RMove ctype -> [RAsm (nDest) newOp (nSource)]
        RMul ctype -> 
          if (isDestMemoryLocation nDest) then
            let
              (moveDest, newloc) = convertDestIntoReg ctype nDest
            in
            [RAsm newloc (RMove ctype) (RLoc nDest), -- Move shit (dest) into new dest
              RAsm newloc (RMul ctype) nSource] -- Multiply shit
              ++ moveDest -- Move shit (dest) back into the real destination
          else [RAsm (nDest) newOp (nSource)]
        _ -> [RAsm (nDest) newOp (nSource)]
        , labelnum)
    --debug = [RAsmText ("parentStackOffset: " ++ show (parentStackOffset) ++ " selfStackOffset: " ++ show selfStackOffset ++ " callerSaveOffset: " ++ show callerSaveOffset)]
  in
    (nDestMov ++ nSourceMov ++ pre ++ body, newlabelnum, rmapinfo)
-- Error
convertSingleExp rmapinfo ((IAsm _ op src), _) _ = 
    error "InstructionSelection: Too many destinations"
convertSingleExp rmapinfo ((IRet retval), _) n = 
  ([RAsm (RReg "eax") (RMove CInt) (convertVal retval rmapinfo), RRet], n, rmapinfo)
  --case retval of
  --  (ILoc (IReg r _)) -> case (r == 0) of
  --    True -> ([RRet], n, rmapinfo)
  --    False -> ([RAsm (RReg "eax") (RMove CInt) (convertVal retval rmapinfo), RRet], n, rmapinfo)
  --  _ -> ([RAsm (RReg "eax") (RMove CInt) (convertVal retval rmapinfo), RRet], n, rmapinfo)

convertSingleExp rmapinfo ((IAsmLabel str), _) n = ([RAsmLabel str], n, rmapinfo)
convertSingleExp rmapinfo ((IAsmJump t str), _) n = ([RAsmJump t str], n, rmapinfo)


convertHeapVal :: StackState -> RLoc -> IVal -> (RVal, [RAsm])
convertHeapVal rmapinfo reservedRegister (ILoc loc) = 
  let 
    (newLoc, newPrefixInstructions) = convertHeap rmapinfo reservedRegister loc
  in
    (RLoc newLoc, newPrefixInstructions)
convertHeapVal rmapinfo _ val = (convertVal val rmapinfo, [])

-- Given a reserved register and ILoc which is possibly a heap,
-- Return an RLoc that contains the value of the original ILoc
-- as well as a list of instructions you need to execute before
-- using that RLoc
convertHeap :: StackState -> RLoc -> ILoc -> (RLoc, [RAsm])
-- Special case for heap
convertHeap rmapinfo reservedRegister (IHeap val ctype) =
  let
    newVal = convertVal val rmapinfo
    -- Pointer is 8 bytes!!
    reservedRegisterRightType = reg32toreg64 reservedRegister
  in
  (RHeap reservedRegisterRightType, [RAsm reservedRegisterRightType (RMove (CPtr ctype)) newVal])
-- General case: Do nothing
convertHeap rmapinfo reservedRegister loc = (convertLoc loc rmapinfo,[])



-- We're adding calls here

determineCalleeSaves :: RegisterMapping -> String -> [RLoc]
determineCalleeSaves regmap functionName = -- calleeSaveRegs --This should be changed back!
  let
    -- calleeSaveRegs MUST be 32-bit!!!!!!!!!!!
    reg32list = List.intersect calleeSaveRegs $ filter isReg (Map.elems regmap)
    reg64list = List.map reg32toreg64 reg32list
  in
    reg64list

determineCallerSaves :: StackState -> Set.Set ILoc -> [RLoc]
determineCallerSaves info live = --callerSaveRegs --This should be changed back!
  let
  -- callerSaveRegs MUST be 32-bit!!!!!!!!!!!
    liveregs = filter isNotIHeap (Set.elems live)
    reg32list = List.intersect callerSaveRegs $ filter isReg (map (\loc -> convertLoc loc info) liveregs)
    reg64list = List.map reg32toreg64 reg32list
  in
    reg64list

getStackArg :: StackState -> ILoc -> RLoc
getStackArg (rmap, parentStackOffset, selfStackOffset, callerSaveOffset) (IInArg i ctype) =
  if i > 5 then RMem (8*(parentStackOffset + i - 6)) else error("IInArg is less than 6")
getStackArg _ _ = error "Calling getStackArg with something that is not an argument"

convertConditional :: StackState -> RLoc -> RVal -> AOp -> LabelNum -> ([RAsm], LabelNum)
convertConditional rmapinfo dest src op n =
    let
        conditional = [RAsm dest (RCmp CBool) src]
        jumpToElse = [RAsmJump (getJumpOp op) (".L" ++ (show (n)))]
        jumpToEnd = [RAsmJump RJump (".L" ++ (show (n + 1)))]
        elseLabel = [RAsmLabel (".L" ++ (show (n)))]
        endLabel = [RAsmLabel (".L" ++ (show (n + 1)))]
        asm1 = [RAsm dest (RMove CBool) (RImm 0)]
        asm2 = [RAsm dest (RMove CBool) (RImm 1)]
    in
        (conditional ++ jumpToElse ++ asm1 ++ jumpToEnd ++ elseLabel ++ asm2 ++ endLabel, n + 2)


isDestMemoryLocation :: RLoc -> Bool
isDestMemoryLocation (RMem _) = True
isDestMemoryLocation (RHeap _) = True
isDestMemoryLocation _ = False

pushCallerSaveDumb = List.map (\reg -> RAsmUnary RPushq (RLoc reg)) callerSaveRegs
popCallerSaveDumb = List.map (\reg -> RAsmUnary RPopq (RLoc reg)) (List.reverse callerSaveRegs)

pushCalleeSave savedRegs = List.map (\reg -> RAsmUnary (RPushq) (RLoc reg)) savedRegs
-- Popping must be done in REVERSE!
popCalleeSave savedRegs = List.map (\reg -> RAsmUnary (RPopq) (RLoc reg)) (reverse savedRegs)


argmap = [
  (RReg "edi"),
  (RReg "esi"),
  (RReg "edx"),
  (RReg "ecx"),
  (RReg "r8d"),
  (RReg "r9d")]

-- This MUST be in 32-bit!! It will be converted to 64-bit later. DO NOT CHANGE!
callerSaveRegs = [
  (RReg "edi"),
  (RReg "esi"),
  (RReg "edx"),
  (RReg "ecx"),
  (RReg "r8d"),
  (RReg "r9d"),
  (RReg "r10d"),
  (RReg "r11d")]

-- This MUST be in 32-bit!! It will be converted to 64-bit later. DO NOT CHANGE!
calleeSaveRegs = [
  (RReg "ebx"),
  (RReg "ebp"),
  (RReg "r12d"),
  (RReg "r13d"),
  (RReg "r14d"),
  (RReg "r15d")]

-- Convert destination to a fucking register
-- We use r13d since r13d is also used for the address of source operands on the heap
-- That will not intefere with binops.
convertDestIntoReg :: CType -> RLoc -> ([RAsm], RLoc)
convertDestIntoReg ctype dest = ([RAsm dest (RMove ctype) (RLoc (RReg "r13d"))], RReg "r13d")

-- Is Intel forcing us to change the source?? :(
-- If Yes: Aww fuck Intel. Why must you force me to make the source a register?
-- If No: Aww fuck Intel. Why must you force me to make the source a register?
checkIfNeedToMoveAndMove :: RLoc -> RVal -> AOp -> ([RAsm], RVal)
checkIfNeedToMoveAndMove dest src op =
  case checkIfNeedToMove dest src op of
    True -> moveSourceToRegister dest src
    False -> ([], src)

-- Aww fuck Intel. Why must you force me to make the source a register?
moveSourceToRegister :: RLoc -> RVal -> ([RAsm], RVal)
moveSourceToRegister dest src =
  -- Return instruction to move shit (src) to r15d, and r15d itself.
  ([RAsm (RReg "r15d") RMoveq src], RLoc (RReg "r15d"))

-- Is Intel forcing us to change the source?? :(
checkIfNeedToMove :: RLoc -> RVal -> AOp -> Bool
-- Unfortunately, we fucked up types so all moves are hacked to movqs
-- Thus mov dest, imm enforces that the destination must be a register
-- Since all immediates are now 64-bit. Fuck!
checkIfNeedToMove _ src@(RImm n) ADiv = True
checkIfNeedToMove _ src@(RImm n) AMod = True
checkIfNeedToMove dest@(RMem _) src@(RImm n) _ = True
checkIfNeedToMove dest@(RHeap _) src@(RImm n) _ = True
-- Mem to mem moves
checkIfNeedToMove dest@(RMem _) src@(RLoc (RMem n)) _ = True
checkIfNeedToMove dest@(RHeap _) src@(RLoc (RMem n)) _ = True
checkIfNeedToMove dest@(RMem _) src@(RLoc (RHeap n)) _ = True
checkIfNeedToMove dest@(RHeap _) src@(RLoc (RHeap n)) _ = True
checkIfNeedToMove _ src@(RLoc (RReg reg)) op =
  let
    reservedRegisters = case op of
      ADiv -> ["eax","edx"]
      AMod -> ["eax","edx"]
      AMul -> ["eax"]
      _ -> []
  in
    elem reg reservedRegisters
checkIfNeedToMove _ _ _ = False

convertVal :: IVal -> StackState -> RVal
convertVal (ILoc x) rmapinfo = (RLoc (convertLoc x rmapinfo))
convertVal (IImm n) rmapinfo = (RImm n)

convertLoc :: ILoc -> StackState -> RLoc
-- There is a special case here, where IReg can only be eax
convertLoc (IInArg i ctype) rmapinfo = getStackArg rmapinfo (IInArg i ctype)
convertLoc (IReg n ctype) rmapinfo = case (n < length registers, n== (fromIntegral eflagsRegisterNumber)) of
    (True, False) -> RReg (registers !! n)
    (_, True) -> (RReg "eflags")
    _ -> error("IReg out of range" ++ show n)
convertLoc (IMem n ctype) rmapinfo = error "InstructionSelection: How is there a mem referance here?" -- (RMem n)
convertLoc (ITemp n ctype) (state@(rmap, parentStackOffset, selfStackOffset, callerSaveOffset)) =
  reindexTempVarForStack state (rmap Map.! n)
convertLoc (IHeap n ctype) rmapinfo = error "IHeap should have been handled by convertSingleExp, not here since we can't do register moves here."
convertLoc loc _ = error ("Cannot convertLoc unmatched case:" ++ show loc)

convertOp :: AOp -> CType -> ROp
convertOp op ctype = 
  case op of   
    AAdd -> RAdd ctype
    ASub -> RSub ctype
    ADiv -> RDiv ctype
    AMul -> RMul ctype
    AMod -> RMod ctype
    ANop -> RMove ctype
    ABAnd -> RBAnd ctype
    ABXor -> RBXor ctype
    ABOr -> RBOr ctype
    AShiftL -> RSal ctype
    AShiftR -> RSar ctype
    ACmp -> RCmp ctype
    ACmpq -> RCmpq
    AAddq -> RAddq
    ASubq -> RSubq

getJumpOp :: AOp -> JumpType
getJumpOp op = 
  case op of   
    ALess -> RJL
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

isReg :: RLoc -> Bool
isReg (RReg _) = True
isReg _ = False

findRestoreLivesets :: [(IAsm, Set.Set ILoc)] -> Set.Set ILoc
findRestoreLivesets [] = error "Reached the end of the program without a restore from a save"
findRestoreLivesets (x:xs) = 
  case x of
    (IRestore, live) -> live
    _ -> findRestoreLivesets xs

reg32toreg64 :: RLoc -> RLoc
reg32toreg64 (RReg reg) = RReg (reg32toreg64string reg)
reg32toreg64 other = other
reg32toreg64string :: String -> String
reg32toreg64string reg = 
  case reg of
    "eax" ->   "rax"
    "ebx" ->   "rbx"
    "ecx" ->   "rcx"
    "edx" ->   "rdx"
    "esi" ->   "rsi"
    "edi" ->   "rdi"
    "ebp" ->   "rbp"
    "r8d" ->  "r8"
    "r9d" ->  "r9"
    "r10d" ->  "r10"
    "r11d" -> "r11"
    "r12d" -> "r12"
    "r13d" -> "r13"
    "r14d" -> "r14"
    "r15d" -> "r15" 

-- Calculate stackSize, which is the number of stack variables
-- For unknown reasons, register allocation and distribution refuses to use
-- stack space in increasing order. In zirconium-return-pass.l3, it uses
-- 8(rsp), 32(rsp), ... without using the spaces in the middle.
countStackVars :: RegisterMapping -> Int
countStackVars rmap =
  let
    stackVarRegList = Map.elems rmap -- List of stack vars and register vars
    stackVarList = List.filter isStack stackVarRegList -- Filter out only stack vars
    stackOffsetList = List.map (\(RMem n) -> n) stackVarList
    maxStackOffset = case length stackOffsetList of
      0 -> 0
      _ -> (maximum stackOffsetList) + 8 -- Why plus 8? Think about this example: If a program uses 0(%rsp), you need to sub 8 bytes for that register.
    numStackVars = (maxStackOffset `div` 8)
    answer = numStackVars
  in
    answer

isStack :: RLoc -> Bool
isStack (RMem _) = True
isStack _ = False

-- Given an ILoc, tells you the ctype!
getLocType :: ILoc -> CType
getLocType (IReg n ctype)   = ctype 
getLocType (ITemp n ctype)  = ctype
getLocType (IMem n ctype)   = ctype 
getLocType (IArg n ctype)   = ctype 
getLocType (IInArg n ctype) = ctype
getLocType (IHeap n ctype) = ctype

isNotIHeap :: ILoc -> Bool
isNotIHeap (IHeap _ _) = False
isNotIHeap _ = True

