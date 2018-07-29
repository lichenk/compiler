module Compile.Types.JumpAbstractAssembly where
import Compile.Types.TAST
import Compile.Types.Ops
import Compile.Types.AbstractAssembly
import Compile.Types.Assembly

data JAsm = JAsm {aAssign :: [JLoc]
                 ,aOp     :: AOp
                 ,aArgs   :: [JVal]
                 }
          | JRet JVal
          | JLabel Int [JLoc]
          | JJmp Int [JVal] JumpType
          | JCall JLoc String [JVal]

data JVal = JLoc JLoc
          | JImm Int deriving (Eq, Ord)

data JLoc = JReg Int Int CType
          | JTemp Int Int CType
          | JMem Int Int CType
          | JInArg Int Int CType
          | JArg Int Int CType
          | JHeap JVal CType

instance Show JAsm where
  show (JAsm dest ANop [src]) = (show dest) ++ " <-- " ++ (show src) ++ "\n"
  show (JAsm dest asnop [src]) = (show dest) ++ " <-- " ++ (show asnop) ++ (show src) ++ "\n"
  show (JAsm dest asnop [src1, src2]) = 
    (show dest) ++ " <-- " ++ (show src1) ++ " " ++ (show asnop) ++ " " ++ (show src2) ++ "\n"
  show (JRet retval) = "ret " ++ (show retval) ++ "\n"
  show (JCall dest f arglist) = (show dest) ++ " <-- call " ++ f ++ "(" ++ (show arglist) ++ ")\n"
  show (JLabel label vars) = ".L" ++ (show label) ++ " " ++ (show vars) ++ "\n"
  show (JJmp target vars jmptype) = (show jmptype) ++ " " ++ (show target) ++ " " ++ (show vars) ++ "\n"

instance Show JVal where
  show (JLoc loc) = show loc
  show (JImm n) = show n

instance Show JLoc where
  show (JReg n g _) = "%r" ++ (show n) ++ ":" ++ (show g)
  show (JTemp n g _) = "%t" ++ (show n) ++ ":" ++ (show g)
  show (JMem n g _) = "M[" ++ (show n) ++ ":" ++ (show g) ++ "]"
  show (JArg n g _) = "outarg " ++ (show n) ++ ":" ++ (show g)
  show (JInArg n g _) = "inarg " ++ (show n) ++ ":" ++ (show g)
  show (JHeap v _) = "HEAP[" ++ (show v) ++ "]"

-- We define our own instances of eq and ord so that the types don't matter
instance Eq JLoc where
  (JReg r1 g1 _) == (JReg r2 g2 _) = (r1, g1) == (r2, g2)
  (JTemp n1 g1 _) == (JTemp n2 g2 _) = (n1, g1) == (n2, g2)
  (JMem n1 g1 _) == (JMem n2 g2 _) = (n1, g1) == (n2, g2)
  (JInArg n1 g1 _) == (JInArg n2 g2 _) = (n1, g1) == (n2, g2)
  (JArg n1 g1 _) == (JArg n2 g2 _) = (n1, g1) == (n2, g2)
  (JHeap l1 _) == (JHeap l2 _) = l1 == l2
  _ == _ = False

instance Ord JLoc where
  compare (JReg r1 g1 _) (JReg r2 g2 _) = compare (r1, g1) (r2, g2)
  compare (JTemp n1 g1 _) (JTemp n2 g2 _) = compare (n1, g1) (n2, g2)
  compare (JMem n1 g1 _) (JMem n2 g2 _) = compare (n1, g1) (n2, g2)
  compare (JInArg n1 g1 _) (JInArg n2 g2 _) = compare (n1, g1) (n2, g2)
  compare (JArg n1 g1 _) (JArg n2 g2 _) = compare (n1, g1) (n2, g2)
  compare (JHeap l1 _) (JHeap l2 _) = compare l1 l2
-- Arbitrarily say registers are greatest, then temps, then mem, the inarg, then arg, then heap
  compare (JReg _ _ _) _ = GT 
  compare _ (JReg _ _ _) = LT
  compare (JTemp _ _ _) _ = GT
  compare _ (JTemp _ _ _) = LT
  compare (JMem _ _ _) _ = GT
  compare _ (JMem _ _ _) = LT
  compare (JInArg _ _ _) _ = GT
  compare _ (JInArg _ _ _) = LT
  compare (JArg _ _ _) _ = GT
  compare _ (JArg _ _ _) = LT