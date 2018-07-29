{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>
                Rokhini Prabhu <rokhinip@andrew.cmu.edu>

   Abstract Assembly operations
-}
module Compile.Types.Ops where

-- For abstract assembly
data AOp = AAdd
         | AAddq
         | ASub
         | ASubq
         | ADiv
         | AMul
         | AMod
         | ANop 
         | ANopDontPropagate
         | AShiftL
         | AShiftR 
         | ALess 
         | ALeq 
         | AGeq 
         | AGreater
         | AEq 
         | ANeq 
         | ABAnd 
         | ABXor 
         | ABOr 
         | ALAnd 
         | ALOr
         | ACmpq
         | ACmp deriving Eq

-- For AST
data BinopType = PolyEqual| RelativeEqual | LogicalBinop | IntegerBinop
data Binop = Add | Sub | Mul | Div | Mod 
           | ShiftL | ShiftR | Less | Leq | Geq | Greater
           | Eq | Neq | BAnd | BXor | BOr | LAnd | LOr deriving Eq
data Unop = Neg | Flip | Not
data Postop = Inc | Dec deriving Eq
data Asnop = AsnOp Binop 
           | Equal deriving Eq
data Ternop = Tern

instance Show AOp where
  show AAdd = "+"
  show AAddq = "+"
  show ASub = "-"
  show ASubq = "-"
  show ADiv = "/"
  show AMul = "*"
  show AMod = "%"
  show ANop = "[move]"
  show ANopDontPropagate = "[movedontpropagate]"
  show AShiftL = "<<"
  show AShiftR = ">>"
  show ALess = "<"
  show ALeq = "<="
  show AGeq = ">="
  show AGreater = ">"
  show AEq = "=="
  show ANeq = "!="
  show ABAnd = "&"
  show ABXor = "^"
  show ABOr = "|"
  show ALAnd = "&&"
  show ALOr = "||"
  show ACmp = "cmp"
  show ACmpq = "cmpq"

instance Show Binop where
  show Mul = "*"
  show Add = "+"
  show Sub = "-"
  show Div = "/"
  show Mod = "%"
  show ShiftL = "<<"
  show ShiftR = ">>"
  show Less = "<"
  show Leq = "<="
  show Geq = ">="
  show Greater = ">"
  show Eq = "=="
  show Neq = "!="
  show BAnd = "&"
  show BXor = "^"
  show BOr = "|"
  show LAnd = "&&"
  show LOr = "||"

instance Show Unop where
  show Neg = "-"
  show Flip = "~"
  show Not = "!"

instance Show Postop where
  show Inc = "++"
  show Dec = "--"

instance Show Asnop where
  show (AsnOp binop) = (show binop) ++ "="
  show Equal = "="

