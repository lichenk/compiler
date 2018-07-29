{
module Compile.Lexer where

import Debug.Trace
import Compile.Types.Ops
}
%wrapper "basic"

$decdigit = [0-9]
$decstart = [1-9]
$hexdigit = [0-9A-Fa-f]
$opstart = [\=\+\-\*\/\%\&\^\|\<\>\!\~]
$opletter = [\=\&\|\<\>\-\+]
$identstart = [A-Za-z_]
$identletter = [A-Za-z0-9_]


tokens :-

  continue {\s -> TokReserved}
  typedef {\s -> TokTypedef}
  break {\s -> TokReserved}
  assert {\s -> TokAssert}
  NULL {\s -> TokNULL}
  alloc {\s -> TokAlloc}
  alloc_array {\s -> TokAllocArr}
  void {\s -> TokVoid}
  char {\s -> TokReserved}
  string {\s -> TokReserved}
  struct {\s -> TokStruct}

  ","   {\s -> TokComma}
  "."   {\s -> TokDot}
  "!"   {\s -> TokNot}
  "~"   {\s -> TokFlip}
  "-"   {\s -> TokMinus}
  "+"   {\s -> TokPlus}
  "*"   {\s -> TokTimes}
  "/"   {\s -> TokDiv}
  "%"   {\s -> TokMod}
  "<<"  {\s -> TokLShift}
  ">>"  {\s -> TokRShift}
  "<"   {\s -> TokLess}
  ">"   {\s -> TokGreater}
  ">="  {\s -> TokGeq}
  "<="  {\s -> TokLeq}
  "=="  {\s -> TokEquality}
  "!="  {\s -> TokNeq}
  "&"   {\s -> TokBAnd}
  "^"   {\s -> TokBXor}
  "|"   {\s -> TokBOr}
  "&&"  {\s -> TokLAnd}
  "||"  {\s -> TokLOr}
  "="   {\s -> TokAsgnop Equal}
  "+="  {\s -> TokAsgnop (AsnOp Add)}
  "-="  {\s -> TokAsgnop (AsnOp Sub)}
  "*="  {\s -> TokAsgnop (AsnOp Mul)}
  "/="  {\s -> TokAsgnop (AsnOp Div)}
  "%="  {\s -> TokAsgnop (AsnOp Mod)}
  "<<=" {\s -> TokAsgnop (AsnOp ShiftL)}
  ">>=" {\s -> TokAsgnop (AsnOp ShiftR)}
  "&="  {\s -> TokAsgnop (AsnOp BAnd)}
  "|="  {\s -> TokAsgnop (AsnOp BOr)}
  "^="  {\s -> TokAsgnop (AsnOp BXor)}
  "++"  {\s -> TokPostop Inc}
  "--"  {\s -> TokPostop Dec}
  "->"  {\s -> TokArrow}

  \/\/.*\n ;
  $white+ ;
  0 {\s -> TokDec 0}
  $decstart $decdigit* {\s -> TokDec (read s)}
  0 [xX] $hexdigit+ {\s -> TokHex (read s)}
  if {\s -> TokIf}
  else {\s -> TokElse}
  while {\s -> TokWhile}
  for {\s -> TokFor}
  return {\s -> TokReturn}
  true {\s -> TokTrue}
  false {\s -> TokFalse}
  int {\s -> TokInt}
  bool {\s -> TokBool}
  $identstart $identletter* {\s -> TokIdent s}
  [\(] {\s -> TokLParen}
  [\)] {\s -> TokRParen}
  [\{] {\s -> TokLBrace}
  [\}] {\s -> TokRBrace}
  [\[] {\s -> TokLSquare}
  [\]] {\s -> TokRSquare}
  [\;] {\s -> TokSemi}
  [:] {\s -> TokColon}
  [\?] {\s -> TokQuestion}

{
-- Each action has type :: String -> Token
-- The token type:
data Token =
  TokAssert |
  TokArrow |
  TokDot |
  TokAlloc |
  TokAllocArr |
  TokStruct |
  TokVoid |
  TokTypedef |
  TokComma |
  TokLParen |
  TokRParen |
  TokLBrace |
  TokRBrace |
  TokLSquare |
  TokRSquare |
  TokSemi |
  TokColon |
  TokQuestion |
  TokDec Int |
  TokHex Int |
  TokIdent String |
  TokVarIdent String |
  TokTypeIdent String |
  TokStructIdent String |
  TokIf |
  TokElse |
  TokWhile |
  TokFor |
  TokReturn |
  TokTrue |
  TokFalse |
  TokInt |
  TokBool |
  TokMinus |
  TokNot |
  TokFlip |
  TokAsgnop Asnop |
  TokPlus |
  TokTimes |
  TokDiv |
  TokMod |
  TokLShift |
  TokRShift |
  TokLess |
  TokGreater |
  TokGeq |
  TokLeq |
  TokNeq |
  TokBAnd |
  TokBXor |
  TokBOr |
  TokLAnd |
  TokLOr |
  TokEquality |
  TokPostop Postop |
  TokNULL |
  TokReserved
  deriving (Eq,Show)

removeComments :: String -> String
removeComments s = removeCommentsHelp 0 s

removeCommentsHelp n [] =
  if (n > 0) then error "Unclosed block comment"
  else []
removeCommentsHelp n (x:[]) =
  if (n > 0) then error "Unclosed block comment"
  else x:[]
removeCommentsHelp n (x:y:xs) =
  case [x, y] of
    "/*" -> removeCommentsHelp (n+1) xs
    "*/" -> 
      if (n > 0)
      then (' ':(removeCommentsHelp (n-1) xs))
      else error "Unmatched block comment closer"
    "//" ->
      if (n > 0)
      then removeCommentsHelp n (y:xs)
      else removeComments $ removeLineComment xs
    _ -> 
      if (n > 0)
      then removeCommentsHelp n (y:xs)
      else x : (removeCommentsHelp n (y:xs))

removeLineComment :: String -> String
removeLineComment [] = []
removeLineComment (x:xs) = 
  if (x == '\n') 
  then (x:xs) 
  else removeLineComment xs

lexProgram s = alexScanTokens $ removeComments s
}