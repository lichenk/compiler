module Compile.Types.IntermediaryAssembly where

import Compile.Types.Ops
import Compile.Types.Assembly
import qualified Data.Map as Map
import Compile.Types.TAST

type RegisterColoring = Map.Map ILoc Int
type LabelNum = Int

data IAsm = IAsm {iAssign :: [ILoc]
                 ,iOp     :: AOp
                 ,iArg   :: IVal
                 }
          | IRet IVal
          | IAsmLabel String
          | IAsmJump JumpType String
          | ICall String Int
          | ISave
          | IRestore
          | IAlignStack Int

data ICond = ICond IVal
           | ICondOptimized IVal AOp IVal

data IVal = ILoc ILoc
          | IImm Int 
          | IConflict deriving (Eq, Ord)

data ILoc = IReg Int CType
          | ITemp Int CType
          | IMem Int CType
          | IInArg Int CType
          | IArg Int CType
          | IHeap IVal CType

instance Show IAsm where
  show (IAsm dest ANop src) = (show dest) ++ " <-- " ++ (show src) ++ "\n"
  show (IAsm dest isnop src) = (show isnop) ++ "( " ++ (show src) ++ ", " 
                                                ++ (show dest) ++ " )\n"
  show (IAsmLabel label) = label ++ ":" ++ "\n"
  show (IAsmJump t label) = (show t) ++ "\t" ++ label ++ "\n"
  show (IRet retval) = "ret " ++ show retval ++ "\n"
  show (ICall f _) = "call " ++ f ++ "\n"
  show (ISave) = "save" ++ "\n"
  show (IRestore) = "restore" ++ "\n"
  show (IAlignStack n) = "alignstack " ++ show n ++ "\n"

instance Show ICond where
  show (ICond val) = show val
  show (ICondOptimized val1 op val2) = show val1 ++ show op ++ show val2

instance Show IVal where
  show (ILoc loc) = show loc
  show (IImm n) = show n

instance Show ILoc where
  show (IReg r ctype) = "%r" ++ (show r)
  show (ITemp n ctype) = "%t" ++ (show n)
  show (IMem n ctype) = "Stack[" ++ (show n) ++ "]"
  show (IInArg n ctype) = "inarg " ++ (show n)
  show (IArg n ctype) = "outarg " ++ (show n)
  show (IHeap val ctype) = "Heap[" ++ show val ++ "]"

-- We define our own instances of eq and ord so that the types don't matter
instance Eq ILoc where
  (IReg r1 _) == (IReg r2 _) = r1 == r2
  (ITemp n1 _) == (ITemp n2 _) = n1 == n2
  (IMem n1 _) == (IMem n2 _) = n1 == n2
  (IInArg n1 _) == (IInArg n2 _) = n1 == n2
  (IArg n1 _) == (IArg n2 _) = n1 == n2
  (IHeap l1 _) == (IHeap l2 _) = l1 == l2
  _ == _ = False

instance Ord ILoc where
  compare (IReg r1 _) (IReg r2 _) = compare r1 r2
  compare (ITemp n1 _) (ITemp n2 _) = compare n1 n2
  compare (IMem n1 _) (IMem n2 _) = compare n1 n2
  compare (IInArg n1 _) (IInArg n2 _) = compare n1 n2
  compare (IArg n1 _) (IArg n2 _) = compare n1 n2
  compare (IHeap l1 _) (IHeap l2 _) = compare l1 l2
-- Arbitrarily say registers are greatest, then temps, then mem, the inarg, then arg, then heap
  compare (IReg _ _) _ = GT 
  compare _ (IReg _ _) = LT
  compare (ITemp _ _) _ = GT
  compare _ (ITemp _ _) = LT
  compare (IMem _ _) _ = GT
  compare _ (IMem _ _) = LT
  compare (IInArg _ _) _ = GT
  compare _ (IInArg _ _) = LT
  compare (IArg _ _) _ = GT
  compare _ (IArg _ _) = LT