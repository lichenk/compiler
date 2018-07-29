{
module Compile.HeaderParser where

import Compile.Lexer
import Compile.Types.Ops
import Compile.Types.FAST
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  '('     {TokLParen}
  ')'     {TokRParen}
  '{'     {TokLBrace}
  '}'     {TokRBrace}
  ','     {TokComma}
  ';'     {TokSemi}
  ':'     {TokColon}
  '?'     {TokQuestion}
  dec     {TokDec $$}
  hex     {TokHex $$}
  ident   {TokIdent $$}
  if      {TokIf}
  else    {TokElse}
  while   {TokWhile}
  for     {TokFor}
  ret     {TokReturn}
  true    {TokTrue}
  false   {TokFalse}
  int     {TokInt}
  bool    {TokBool}
  void    {TokVoid}
  typedef {TokTypedef}
  '-'     {TokMinus}
  '!'     {TokNot}
  '~'     {TokFlip}
  '+'     {TokPlus}
  '*'     {TokTimes}
  '/'     {TokDiv}
  '%'     {TokMod}
  '<<'    {TokLShift}
  '>>'    {TokRShift}
  '<'     {TokLess}
  '>'     {TokGreater}
  '>='    {TokGeq}
  '<='    {TokLeq}
  '!='    {TokNeq}
  '&'     {TokBAnd}
  '^'     {TokBXor}
  '|'     {TokBOr}
  '&&'    {TokLAnd}
  '||'    {TokLOr}
  '=='    {TokEquality}    
  asgnop  {TokAsgnop $$}
  postop  {TokPostop $$}
  assert  {TokAssert}
  kill    {TokReserved}

%%
{- Program -}
Program : {- Empty -} {[]}
        | Gdecl Program {$1 : $2}

{- Global declaration -}
Gdecl : Fdecl {$1}
      | Typedef {$1}

{- Function declaration -}
Fdecl : Type ident Paramlist ';' {Fdecl $1 $2 $3}

{- Parameter -}
Param : Type ident {Param $1 $2}

{- Parameter list not including the first parameter (to avoid shift/reduce conflict) -}
Paramlistfollow : {- Empty -} {[]}
                | ',' Param Paramlistfollow {$2 : $3}

{- Parameter list -}
Paramlist : '(' ')' {[]}
          | '(' Param Paramlistfollow ')' {$2 : $3}

{- Typedef in C0 language-}
Typedef : typedef Type ident ';' {Typedef $2 $3}

{- CTypeIdent is for types which have been defined using typedef -}
Type  : int {CInt}
      | bool {CBool}
      | ident {CTypeIdent $1}
      | void {CVoid}
{
parseError :: [Token] -> a
parseError t = error ("Parse Error: " ++ show t)
}