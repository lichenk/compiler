{- Clean assembly sounds about as oxymoronic as clean coal -}
module Compile.CleanAssembly (cleanProgram) where

import Compile.Types
import Debug.Trace
import Data.List
import Compile.Constants

cleanProgram :: [RAsm] -> [RAsm]
cleanProgram prog = stripJumps $ removeIdentities prog

{-
-- This is actually just a big list of powers of two.  Nothing special, just precomputing them.
powersOfTwo = [1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536,131072,262144,524288,1048576,2097152,4194304,8388608,16777216,33554432,67108864,134217728,268435456,536870912,1073741824,2147483648,4294967296,8589934592,17179869184,34359738368,68719476736,137438953472,274877906944,549755813888,1099511627776,2199023255552,4398046511104,8796093022208,17592186044416,35184372088832,70368744177664,140737488355328,281474976710656,562949953421312,1125899906842624,2251799813685248,4503599627370496,9007199254740992,18014398509481984,36028797018963968,72057594037927936,144115188075855872,288230376151711744,576460752303423488,1152921504606846976,2305843009213693952,4611686018427387904,9223372036854775808,18446744073709551616]

multToShift :: [RAsm] -> [RAsm]
multToShift prog =
  let
    transformLine line = 
      case line of
        RAsm loc (RMul t) (RImm n) ->
          case elemIndex n powersOfTwo of
            Nothing -> line
            Just pow -> RAsm loc (RSar t) (RImm pow)
        _ -> line
  in 
    map transformLine prog 
-}

isNeeded :: RAsm -> Bool
isNeeded (RAsm _ op arg) =
  case (op, arg) of
    (RAdd _, RImm 0) -> False
    (RAddq, RImm 0) -> False
    (RSub _, RImm 0) -> False
    (RSubq, RImm 0) -> False
    (RMul _, RImm 1) -> False
    _ -> True
isNeeded _ = True

removeIdentities :: [RAsm] -> [RAsm]
removeIdentities prog = filter isNeeded prog

stripJumps :: [RAsm] -> [RAsm]
stripJumps [] = []
stripJumps ((RAsmJump jtype target):(RAsmLabel label):rest) =
  if (target == label)
  then (RAsmLabel label):(stripJumps rest)
  else (RAsmJump jtype target):(RAsmLabel label):(stripJumps rest)
stripJumps (x:xs) = x:(stripJumps xs)