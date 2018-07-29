module Compile.Types.Assembly where

import Compile.Types.TAST
import Compile.Types.Ops
import Compile.Constants
import qualified Data.Map as Map

type RegisterMapping = Map.Map Int RLoc

data RAsm = RAsm {rAssign :: RLoc
                 ,rOp     :: ROp
                 ,rArg   ::  RVal
                 }
          | RAsmUnary {rOpUnary :: ROpUnary
                      ,rAssignUnary :: RVal
                      }
          | RRet
          | RComment String
          | RAsmText String
          | RAsmLabel String
          | RAsmJump JumpType String
          | RCall String deriving (Eq)

data JumpType = RJump | RJe | RJL | RJLe | RJGe | RJG | RJne deriving (Eq)

data RVal = RLoc RLoc
          | RImm Int deriving (Ord, Eq)

data RLoc = RReg String
          | RHeap RLoc
          | RMem Int deriving (Ord, Eq)

type OpSize = Int

data ROp = RAdd  CType
         | RSub  CType
         | RDiv  CType
         | RMul  CType
         | RMod  CType
         | RSar  CType
         | RSal  CType
         | RMove CType
         | RBAnd CType
         | RBXor CType
         | RBOr  CType
         | RCmp  CType
         | RCmpq
         | RAddq
         | RSubq
         | RMoveq deriving (Eq)

data ROpUnary = RPushq
              | RPopq deriving Eq


instance Show RAsm where
  show (RAsm dest isnop src) = showbinop dest isnop src
  show (RAsmUnary op src) = "\t" ++ (show op) ++ "\t" ++ (show src)
  show (RRet) = "\t" ++ "ret"
  show (RAsmText str) = str
  show (RAsmLabel label) = label ++ ":"
  show (RAsmJump t label) = "\t" ++ (show t) ++ "\t" ++ label
  show (RCall label) = "\t" ++ "call" ++ "\t" ++ headerPrefix ++ label

instance Show JumpType where
  show RJump = "jmp"
  show RJe = "je"
  show RJL = "jl"
  show RJLe = "jle"
  show RJGe = "jge"
  show RJG = "jg"
  show RJne = "jne"

instance Show RVal where
  show (RLoc loc) = show loc
  show (RImm n) = "$" ++ show n

instance Show RLoc where
  show (RReg n) = "%" ++ n
  --show (Temp n) = "%t" ++ (show n)
  show (RMem n) = (show n) ++ "(%rsp)"
  show (RHeap loc) = "0" ++ "(" ++ show loc ++ ")"

instance Show ROp where
  show (RAdd  ctype) = "add" ++ showOpSuffix  ctype
  show (RSub  ctype) = "sub" ++ showOpSuffix  ctype
  show (RDiv  ctype) = "idivl" -- Force these to be for longs only
  show (RMul  ctype) = "imull" -- Force these to be for longs only
  show (RMod  ctype) = "idivl" -- Force these to be for longs only
  show (RSar  ctype) = "sar" ++ showOpSuffix  ctype
  show (RSal  ctype) = "sal" ++ showOpSuffix  ctype
  show (RMove ctype) = "mov" ++ showOpSuffix  ctype
  show (RBAnd ctype) = "and" ++ showOpSuffix  ctype
  show (RBXor ctype) = "xor" ++ showOpSuffix  ctype
  show (RBOr  ctype) = "or" ++ showOpSuffix   ctype
  show (RCmp  ctype) = "cmp" ++ showOpSuffix  ctype
  show (RCmpq) = "cmpq"
  show (RAddq      ) = "addq"
  show (RSubq      ) = "subq"
  show (RMoveq     ) = "movq"

getTypeSize :: CType -> Int
getTypeSize (CInt) = 4
getTypeSize (CBool) = 4
getTypeSize (CVoid) = 4
getTypeSize (CPtr _) = 8
getTypeSize (CArray _) = 8
getTypeSize (CStruct _) = 8
getTypeSize (CAny) = 8
getTypeSize _ = 8

showOpSuffix :: CType -> String
showOpSuffix ctype = case getTypeSize ctype of
  4 -> "l"
  8 -> "q"

instance Show ROpUnary where
  show RPushq = "pushq"
  show RPopq = "popq"

showbinop :: RLoc -> ROp -> RVal -> String
showbinop dest isnop src =
  let
    (newop, destSize, srcSize) = getOpSize isnop dest src
    srcString = showVal srcSize src
    destString = showLoc destSize dest
  in
    "\t" ++ (show newop) ++ "\t" ++ srcString ++ ", " ++ destString

-- Shows loc with register names unfucked to the right size
showLoc :: Int -> RLoc -> String
showLoc size (RReg reg) = show (resizeRLoc size (RReg reg))
showLoc size (RMem n) = show (RMem n)
-- Must use size as 8 below!! Since a pointer is 8 bytes \/
showLoc size (RHeap (RReg reg)) = "(" ++ showLoc 8 (RReg reg) ++ ")"

-- Shows loc with register names unfucked to the right size
showVal :: Int -> RVal -> String
showVal size (RImm n) = show (RImm n)
showVal size (RLoc loc) = showLoc size loc


getOpSize op@(RAdd  ctype) dest src = (op, getTypeSize ctype, getTypeSize ctype)
getOpSize op@(RSub  ctype) dest src = (op, getTypeSize ctype, getTypeSize ctype)
getOpSize op@(RDiv  ctype) dest src = (op, 4, 4)
getOpSize op@(RMul  ctype) dest src = (op, 4, 4)
getOpSize op@(RMod  ctype) dest src = (op, 4, 4)
getOpSize op@(RBAnd ctype) dest src = (op, getTypeSize ctype, getTypeSize ctype)
getOpSize op@(RBXor ctype) dest src = (op, getTypeSize ctype, getTypeSize ctype)
getOpSize op@(RBOr  ctype) dest src = (op, getTypeSize ctype, getTypeSize ctype)
getOpSize op@(RCmp  ctype) dest src = (op, getTypeSize ctype, getTypeSize ctype)
getOpSize op@(RSar  ctype) dest src = (op, getTypeSize ctype, 1)
getOpSize op@(RSal  ctype) dest src = (op, getTypeSize ctype, 1)
getOpSize op@(RCmpq      ) dest src = (op, 8,8)
getOpSize op@(RAddq      ) dest src = (op, 8,8)
getOpSize op@(RSubq      ) dest src = (op, 8,8)
getOpSize op@(RMoveq     ) dest src = (op, 8,8)
getOpSize op@(RMove ctype) dest@(RHeap _) src = (op, getTypeSize ctype, getTypeSize ctype)
getOpSize op@(RMove ctype) dest src@(RLoc (RHeap _)) = (op, getTypeSize ctype, getTypeSize ctype)
getOpSize op@(RMove ctype) _ _ = (RMove (CPtr CNoType), 8,8)



resizeRVal :: Int -> RVal -> RVal
resizeRVal size (RLoc loc) = RLoc (resizeRLoc size loc)
resizeRVal size other = other
resizeRLoc :: Int -> RLoc -> RLoc
resizeRLoc size (RReg reg) = RReg (resizeRegString size reg)
resizeRLoc size other = other
resizeRegString :: Int -> String -> String
resizeRegString size reg = 
  case size of
    1 -> case reg of
          "ecx" -> "cl" -- Only used for shift instructions, but feel free to add other registers
          other -> other -- If you specify a type explicity, you know what you're doing
    4 -> reg
    8 -> case reg of
          "eax" ->  "rax"
          "ebx" ->  "rbx"
          "ecx" ->  "rcx"
          "edx" ->  "rdx"
          "esi" ->  "rsi"
          "edi" ->  "rdi"
          "ebp" ->  "rbp"
          "r8d" ->  "r8"
          "r9d" ->  "r9"
          "r10d" -> "r10"
          "r11d" -> "r11"
          "r12d" -> "r12"
          "r13d" -> "r13"
          "r14d" -> "r14"
          "r15d" -> "r15" 
          "esp"  -> "rsp"
          other -> other -- If you specify a type explicity, you know what you're doing

