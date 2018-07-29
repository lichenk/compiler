module Compile.Types.Assembly where

import Compile.Types.TAST
import Compile.Types.Ops
import qualified Data.Map as Map

type RegisterMapping = Map.Map Int RLoc

data RAsm = RAsm {rAssign :: RLoc
                 ,rOp     :: ROp
                 ,rArg   :: RVal
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
          | RImm Int deriving (Eq)

data RLoc = RReg String CType
          | RHeap RLoc CType
          | RMem Int CType deriving (Ord, Eq)

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
         | RAddq
         | RSubq deriving (Eq)

data ROpUnary = RPushq
              | RPopq deriving Eq


instance Show RAsm where
  show (RAsm dest isnop src) = "\t" ++ (show isnop) ++ "\t" ++ (show src) ++ ", " 
                                                ++ (show dest)
  show (RAsmUnary op src) = "\t" ++ (show op) ++ "\t" ++ (show src)
  show (RRet) = "\t" ++ "ret"
  show (RAsmText str) = str
  show (RAsmLabel label) = label ++ ":"
  show (RAsmJump t label) = "\t" ++ (show t) ++ "\t" ++ label
  show (RCall label) = "\t" ++ "call" ++ "\t" ++ label

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
  show (RReg n ctype) = "%" ++ n
  --show (Temp n) = "%t" ++ (show n)
  show (RMem n ctype) = (show n) ++ "(%rsp)"
  show (RHeap reg ctype) = "0(" ++ show reg ++ ")"




instance Show ROp where
  show (RAdd  ctype) = "add" ++ showOpSuffix ctype
  show (RSub  ctype) = "sub" ++ showOpSuffix ctype
  show (RDiv  ctype) = "idiv" ++ showOpSuffix ctype
  show (RMul  ctype) = "imul" ++ showOpSuffix ctype
  show (RMod  ctype) = "idiv" ++ showOpSuffix ctype
  show (RSar  ctype) = "sar" ++ showOpSuffix ctype
  show (RSal  ctype) = "sal" ++ showOpSuffix ctype
  show (RMove ctype) = "mov" ++ showOpSuffix ctype
  show (RBAnd ctype) = "and" ++ showOpSuffix ctype
  show (RBXor ctype) = "xor" ++ showOpSuffix ctype
  show (RBOr  ctype) = "or" ++ showOpSuffix ctype
  show (RCmp  ctype) = "cmp" ++ showOpSuffix ctype
  show (RAddq      ) = "addq"
  show (RSubq      ) = "subq"

instance Show ROpUnary where
  show (RPushq) = "pushq"
  show (RPopq ) = "popq" 

getTypeSize :: CType -> Int
getTypeSize (CInt) = 4
getTypeSize (CBool) = 4
getTypeSize (CPtr _) = 8
getTypeSize (CArray _) = 8
getTypeSize (CStruct _) = 8
getTypeSize (CNullType) = 8
getTypeSize (CEight) = 8
getTypeSize (CByte) = 1


showOpSuffix :: CType -> String
showOpSuffix ctype = case getTypeSize ctype of
  4 -> "l"
  8 -> "q"

printreg :: RLoc -> String
printreg (RReg reg ctype) = case getTypeSize ctype of
  1 -> reg32toreg8string reg
  4 -> reg
  8 -> reg32toreg64string reg

reg32toreg8string :: String -> String
reg32toreg8string reg = 
  case reg of
    "ecx" ->   "cl"

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

