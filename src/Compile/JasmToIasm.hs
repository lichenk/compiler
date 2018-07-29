module Compile.JasmToIasm where
import Compile.Constants
import Compile.Types
import Compile.Types.JumpAbstractAssembly

convertJasmToIasm :: [JAsm] -> [IAsm]
convertJasmToIasm jasm =
  concat (map convert jasm)

convert :: JAsm -> [IAsm]
convert (JAsm dest ANop [src]) = [IAsm (convertLocs dest) ANop (convertVal src)]
-- Cmp does not really move to a target!
convert (JAsm dest ACmp [(JLoc src1), src2]) = 
  [IAsm [(convertLoc src1)] ACmp (convertVal src2)]
convert (JAsm indest inop [insrc1, insrc2]) = 
  let
    (JAsm dest op [src1, src2]) = optimizeBinop (JAsm indest inop [insrc1, insrc2])
  in
    case isLocsEqualVal dest src1 of
      True ->
        [IAsm (convertLocs dest) op (convertVal src2)]
      False ->
        -- First operand not equal to dest, so we must move
        [IAsm (convertLocs dest) ANop (convertVal src1), IAsm (convertLocs dest) op (convertVal src2)]
convert (JAsm dest op _) = 
  error "Converting from 3 to 2 address form failed, too many sources"
convert (JRet retval) = [IRet (convertVal retval)]
convert (JLabel labelnum ssaArgs) = [IAsmLabel (makeLabel labelnum)]
convert (JJmp labelnum ssaArgs jumpType) =
  [IAsmJump jumpType (makeLabel labelnum)]
convert (JCall dest func arglist) =
  let
    argmoves = reverse $ map moveParamToArg (zip arglist [0..])
    newDest = convertLoc dest
    newDestType = getLocType newDest
    retmove = [IAsm [(convertLoc dest)] ANop (ILoc (IReg 0 newDestType))]
  in
    [ISave, IAlignStack (length arglist)] ++ argmoves ++ [ICall func (length arglist), IRestore] ++ retmove

-- Makes label string out of label number by adding ".L" in front of the number
makeLabel :: Int -> String
makeLabel n =
  labelPrefix ++ show n

isLocsEqualVal :: [JLoc] -> JVal -> Bool
isLocsEqualVal [loc] (JLoc val) = (loc == val)
isLocsEqualVal _ _ = False

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

-- Convert common powers of 2 multiply divide to shift operations
-- optimizeBinop :: JAsm -> JAsm
-- optimizeBinop (JAsm dest ADiv [src1, JImm 2]) = (JAsm dest  AShiftR [src1, JImm 1])
-- optimizeBinop (JAsm dest ADiv [src1, JImm 4]) = (JAsm dest  AShiftR [src1, JImm 2])
-- optimizeBinop (JAsm dest ADiv [src1, JImm 8]) = (JAsm dest  AShiftR [src1, JImm 3])
-- optimizeBinop (JAsm dest ADiv [src1, JImm 16])= (JAsm dest AShiftR [src1, JImm 4])
-- optimizeBinop (JAsm dest AMul [src1, JImm 2]) = (JAsm dest  AShiftL [src1, JImm 1])
-- optimizeBinop (JAsm dest AMul [src1, JImm 4]) = (JAsm dest  AShiftL [src1, JImm 2])
-- optimizeBinop (JAsm dest AMul [src1, JImm 8]) = (JAsm dest  AShiftL [src1, JImm 3])
-- optimizeBinop (JAsm dest AMul [src1, JImm 16])= (JAsm dest AShiftL [src1, JImm 4])
optimizeBinop x = x


-- Moves function parameters to the right argument registers/argument stack location
-- arg: argument value
-- i: Argument number
moveParamToArg :: (JVal, Int) -> IAsm
moveParamToArg (arg, i) =
  let
    newArgVal = convertVal arg
    newArgValType = getValType newArgVal
  in
    IAsm [colorOutArg i newArgValType] ANop newArgVal


convertLocs :: [JLoc] -> [ILoc]
convertLocs xs = map convertLoc xs

convertVal :: JVal -> IVal
convertVal (JLoc x) = (ILoc (convertLoc x))
convertVal (JImm n) = (IImm n)

convertLoc :: JLoc -> ILoc
convertLoc (JReg n g ctype) = (IReg n ctype)
convertLoc loc@(JTemp n g ctype)  = 
  case (g == 0) of
    True -> (ITemp n ctype)
    False -> error ("Bug in deSSA: Temp generation is not 0 at:" ++ show loc)
convertLoc (JMem n g ctype) = (IMem n ctype)
convertLoc (JArg n g ctype) = (colorOutArg n ctype)
convertLoc (JInArg n g ctype) = (IInArg n ctype)
convertLoc (JHeap val ctype) = IHeap (convertVal val) ctype

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
