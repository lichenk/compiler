{- 
    3 address to 2 address
-}

module Compile.IntermediaryASMConvert (convertTo2AddressFrom3Address) where

import Compile.Types
import Debug.Trace

-- Given the argument number and the type, outputs an ILoc giving the argument register/stack location
-- Usage: I want to know where to put arg3 before calling the function f(int arg1,int arg2, bool arg3)
-- Then, I call "colorOutArg 3 CBool" to find out where to put it.
colorOutArg :: Int -> CType -> ILoc
colorOutArg n ctype =
  case n of
    0 -> IReg 5 ctype --edi
    1 -> IReg 4 ctype --esi
    2 -> IReg 3 ctype --edx
    3 -> IReg 2 ctype --ecx
    4 -> IReg 7 ctype --r8
    5 -> IReg 8 ctype --r9
    m -> IArg m ctype

-- Intel, in their infinite wisdom, decides to use 2-address format
-- Bow to them and convert our 3-address form to their holy 2-address format
convertTo2AddressFrom3Address :: LabelNum -> [AAsm] -> ([IAsm], LabelNum)
convertTo2AddressFrom3Address n [] = ([], n)
convertTo2AddressFrom3Address n (x:xs) =
    let
        (first, n') = convertSingleExp n x
        (rest, n'') = convertTo2AddressFrom3Address n' xs
    in
        (first ++ rest, n'')
-- Convert common powers of 2 multiply divide to shift operations
optimizeBinop (AAsm dest ADiv [src1, AImm 2]) = (AAsm dest  AShiftR [src1, AImm 1])
optimizeBinop (AAsm dest ADiv [src1, AImm 4]) = (AAsm dest  AShiftR [src1, AImm 2])
optimizeBinop (AAsm dest ADiv [src1, AImm 8]) = (AAsm dest  AShiftR [src1, AImm 3])
optimizeBinop (AAsm dest ADiv [src1, AImm 16]) = (AAsm dest AShiftR [src1, AImm 4])
optimizeBinop (AAsm dest AMul [src1, AImm 2]) = (AAsm dest  AShiftL [src1, AImm 1])
optimizeBinop (AAsm dest AMul [src1, AImm 4]) = (AAsm dest  AShiftL [src1, AImm 2])
optimizeBinop (AAsm dest AMul [src1, AImm 8]) = (AAsm dest  AShiftL [src1, AImm 3])
optimizeBinop (AAsm dest AMul [src1, AImm 16]) = (AAsm dest AShiftL [src1, AImm 4])
optimizeBinop x = x

convertComparisonToInvertedJump :: AOp -> JumpType
convertComparisonToInvertedJump ALess    = RJGe
convertComparisonToInvertedJump ALeq     = RJG
convertComparisonToInvertedJump AGeq     = RJL
convertComparisonToInvertedJump AGreater = RJLe
convertComparisonToInvertedJump AEq      = RJne
convertComparisonToInvertedJump ANeq     = RJe

convertSingleExp :: LabelNum -> AAsm -> ([IAsm], LabelNum)
-- Move instruction
convertSingleExp n (AAsm dest ANopDontPropagate [src]) = ([IAsm (convertLocs dest) ANop (convertVal src)], n)
convertSingleExp n (AAsm dest ANop [src]) = ([IAsm (convertLocs dest) ANop (convertVal src)], n)
-- Add a move in front of a binary instruction
convertSingleExp n (AAsm indest inop [insrc1, insrc2]) = 
  let
    (AAsm dest op [src1, src2]) = optimizeBinop (AAsm indest inop [insrc1, insrc2])
  in
    ([IAsm (convertLocs dest) ANop (convertVal src1), IAsm (convertLocs dest) op (convertVal src2)], n)
-- Error
convertSingleExp _ instr@(AAsm dest op _) = 
    error ("Converting from 3 to 2 address form failed, too many sources " ++ (show instr))
-- If
convertSingleExp n (AIf (ACond val) asmIf asmElse) =
    let
        -- Note: val must be a bool
        conditional = [IAsm ([IReg 14 CBool]) ANop (convertVal val), IAsm ([IReg 14 CBool]) ACmp (IImm 0)]
        jumpToElse = [IAsmJump RJe (".L" ++ (show (n)))]
        jumpToEnd = [IAsmJump RJump (".L" ++ (show (n + 1)))]
        elseLabel = [IAsmLabel (".L" ++ (show (n)))]
        endLabel = [IAsmLabel (".L" ++ (show (n + 1)))]
        (asm1, num1) = convertTo2AddressFrom3Address (n+2) asmIf
        (asm2, num2) = convertTo2AddressFrom3Address num1 asmElse
    in
        (conditional ++ jumpToElse ++ asm1 ++ jumpToEnd ++ elseLabel ++ asm2 ++ endLabel, num2)
convertSingleExp n (AIf (ACondOptimized val1 op val2) asmIf asmElse) =
    let
        valType = getValType (convertVal val1)
        jmpType = convertComparisonToInvertedJump op
        conditional = [IAsm ([IReg 14 valType]) ANop (convertVal val1), IAsm ([IReg 14 valType]) ACmp (convertVal val2)]
        jumpToElse = [IAsmJump jmpType (".L" ++ (show (n)))]
        jumpToEnd = [IAsmJump RJump (".L" ++ (show (n + 1)))]
        elseLabel = [IAsmLabel (".L" ++ (show (n)))]
        endLabel = [IAsmLabel (".L" ++ (show (n + 1)))]
        (asm1, num1) = convertTo2AddressFrom3Address (n+2) asmIf
        (asm2, num2) = convertTo2AddressFrom3Address num1 asmElse
    in
        (conditional ++ jumpToElse ++ asm1 ++ jumpToEnd ++ elseLabel ++ asm2 ++ endLabel, num2)
-- While
convertSingleExp n (AWhile (ACond val) asmBody) =
    let
        conditional = [IAsm ([IReg 14 CBool]) ANop (convertVal val), IAsm ([IReg 14 CBool]) ACmp (IImm 0)]
        jumpToBegin = [IAsmJump RJump (".L" ++ (show (n)))]
        jumpToEnd = [IAsmJump RJe (".L" ++ (show (n + 1)))]
        beginLabel = [IAsmLabel (".L" ++ (show (n)))]
        endLabel = [IAsmLabel (".L" ++ (show (n + 1)))]
        (asm, num1) = convertTo2AddressFrom3Address (n+2) asmBody
    in
        (beginLabel ++ conditional ++ jumpToEnd ++ asm ++ jumpToBegin ++ endLabel, num1)
convertSingleExp n (AWhile (ACondOptimized val1 op val2) asmBody) =
    let
        valType = getValType (convertVal val1)
        jmpType = convertComparisonToInvertedJump op
        conditional = [IAsm ([IReg 14 valType]) ANop (convertVal val1), IAsm ([IReg 14 valType]) ACmp (convertVal val2)]
        jumpToBegin = [IAsmJump RJump (".L" ++ (show (n)))]
        jumpToEnd = [IAsmJump jmpType (".L" ++ (show (n + 1)))]
        beginLabel = [IAsmLabel (".L" ++ (show (n)))]
        endLabel = [IAsmLabel (".L" ++ (show (n + 1)))]
        (asm, num1) = convertTo2AddressFrom3Address (n+2) asmBody
    in
        (beginLabel ++ conditional ++ jumpToEnd ++ asm ++ jumpToBegin ++ endLabel, num1)
convertSingleExp n (ACall dest func arglist) = -- ([ICall func i], n)
    let
        argmoves = reverse $ map moveParamToArg (zip arglist [0..])
        newDest = convertLoc dest
        newDestType = getLocType newDest
        retmove = [IAsm [(convertLoc dest)] ANop (ILoc (IReg 0 newDestType))]
    in
        ([ISave, IAlignStack (length arglist)] ++ argmoves ++ [ICall func (length arglist), IRestore] ++ retmove, n)

-- Return instruction
convertSingleExp n (ARet retval) = ([IRet (convertVal retval)], n)

-- Moves function parameters to the right argument registers/argument stack location
-- arg: argument value
-- i: Argument number
moveParamToArg :: (AVal, Int) -> IAsm
moveParamToArg (arg, i) =
    let
        newArgVal = convertVal arg
        newArgValType = getValType newArgVal
    in
        IAsm [colorOutArg i newArgValType] ANop newArgVal


convertLocs :: [ALoc] -> [ILoc]
convertLocs xs = map convertLoc xs

convertVal :: AVal -> IVal
convertVal (ALoc x) = (ILoc (convertLoc x))
convertVal (AImm n) = (IImm n)

convertLoc :: ALoc -> ILoc
convertLoc (AReg n ctype) = (IReg n ctype)
convertLoc (ATemp n ctype) = (ITemp n ctype)
convertLoc (AMem n ctype) = (IMem n ctype)
convertLoc (AArg n ctype) = (colorOutArg n ctype)
convertLoc (AInArg n ctype) = (IInArg n ctype)
convertLoc (AHeap val ctype) = IHeap (convertVal val) ctype

-- Given an IVal, tells you the ctype!
getValType :: IVal -> CType
getValType (IImm n) = CEightByte
getValType (ILoc x) = getLocType x

-- Given an ILoc, tells you the ctype!
getLocType :: ILoc -> CType
getLocType (IReg n ctype)   = ctype 
getLocType (ITemp n ctype)  = ctype
getLocType (IMem n ctype)   = ctype 
getLocType (IArg n ctype)   = ctype 
getLocType (IInArg n ctype) = ctype
getLocType (IHeap n ctype) = ctype
