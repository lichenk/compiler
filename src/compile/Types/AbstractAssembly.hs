{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Defines a flat abstract assembly.
-}
module Compile.Types.AbstractAssembly where
import Compile.Types.TAST
import Compile.Types.Ops

data AAsm = AAsm {aAssign :: [ALoc]
                 ,aOp     :: AOp
                 ,aArgs   :: [AVal]
                 }
          | AIf ACond [AAsm] [AAsm]
          | AWhile ACond [AAsm]
          | ARet AVal
          | AComment String
          | ACall ALoc String [AVal]
          | ASave Int
          | ARestore Int

data ACond = ACond AVal
           | ACondOptimized AVal AOp AVal

data AVal = ALoc ALoc
          | AImm Int deriving (Eq, Ord)

data ALoc = AReg Int CType
          | ATemp Int CType
          | AMem Int CType
          | AInArg Int CType
          | AArg Int CType
          | AHeap AVal CType

instance Show AAsm where
  show (AAsm dest ANop [src]) = (show dest) ++ " <-- " ++ (show src) ++ "\n"
  show (AAsm dest ANopDontPropagate [src]) = (show dest) ++ " <-- " ++ (show src) ++ " DontPropagate\n"
  show (AAsm dest asnop [src1, src2]) = (show dest) ++ " <-- " ++ (show src1) ++ " " 
                                        ++ (show asnop) ++ " " ++ (show src2) ++ "\n"
  show (AIf cond asm1 asm2) = "If " ++ (show cond) ++ "\n"
                                  ++ "Then " ++ (show asm1) ++ "\n"
                                  ++ "Else " ++ (show asm2) ++ "\n"
  show (AWhile cond asm) = "While " ++ (show cond) ++ "\n"
                                  ++ "Body " ++ (show asm) ++ "\n"
  show (ARet retval) = "ret " ++ (show retval) ++ "\n"
  show (ACall dest f arglist) = (show dest) ++ " <-- call " ++ f ++ "(" ++ (show arglist) ++ ")\n"
  show (ASave i) = "save " ++ (show i) ++ "\n"
  show (ARestore i) = "restore " ++ (show i) ++ "\n"

instance Show ACond where
  show (ACond val) = show val
  show (ACondOptimized val1 op val2) = show val1 ++ show op ++ show val2

instance Show AVal where
  show (ALoc loc) = show loc
  show (AImm n) = show n

instance Show ALoc where
  show (AReg n _) = "%r" ++ (show n)
  show (ATemp n _) = "%t" ++ (show n)
  show (AMem n _) = "M[" ++ (show n) ++ "]"
  show (AArg n _) = "outarg " ++ (show n)
  show (AInArg n _) = "inarg " ++ (show n)
  show (AHeap v _) = "HEAP[" ++ (show v) ++ "]"

-- We define our own instances of eq and ord so that the types don't matter
instance Eq ALoc where
  (AReg r1 _) == (AReg r2 _) = r1 == r2
  (ATemp n1 _) == (ATemp n2 _) = n1 == n2
  (AMem n1 _) == (AMem n2 _) = n1 == n2
  (AInArg n1 _) == (AInArg n2 _) = n1 == n2
  (AArg n1 _) == (AArg n2 _) = n1 == n2
  (AHeap l1 _) == (AHeap l2 _) = l1 == l2
  _ == _ = False

instance Ord ALoc where
  compare (AReg r1 _) (AReg r2 _) = compare r1 r2
  compare (ATemp n1 _) (ATemp n2 _) = compare n1 n2
  compare (AMem n1 _) (AMem n2 _) = compare n1 n2
  compare (AInArg n1 _) (AInArg n2 _) = compare n1 n2
  compare (AArg n1 _) (AArg n2 _) = compare n1 n2
  compare (AHeap l1 _) (AHeap l2 _) = compare l1 l2
-- Arbitrarily say registers are greatest, then temps, then mem, the inarg, then arg, then heap
  compare (AReg _ _) _ = GT 
  compare _ (AReg _ _) = LT
  compare (ATemp _ _) _ = GT
  compare _ (ATemp _ _) = LT
  compare (AMem _ _) _ = GT
  compare _ (AMem _ _) = LT
  compare (AInArg _ _) _ = GT
  compare _ (AInArg _ _) = LT
  compare (AArg _ _) _ = GT
  compare _ (AArg _ _) = LT
