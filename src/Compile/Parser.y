{
module Compile.Parser where

import Compile.Lexer
import Compile.Types.Ops
import Compile.Types.FAST
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  '['     {TokLSquare}
  ']'     {TokRSquare}
  '('     {TokLParen}
  ')'     {TokRParen}
  '{'     {TokLBrace}
  '}'     {TokRBrace}
  ','     {TokComma}
  ';'     {TokSemi}
  ':'     {TokColon}
  '?'     {TokQuestion}
  '.'     {TokDot}
  '->'    {TokArrow}
  alloc   {TokAlloc}
  alloc_array   {TokAllocArr}
  struct  {TokStruct}
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
  NULL    {TokNULL}
  kill    {TokReserved}


%nonassoc THEN
%nonassoc else

%right '?' ':'
%left '||'
%left '&&'
%left '|'
%left '^'
%left '&'
%left '==' '!='
%left '<' '>' '<=' '>='
%left '>>' '<<'
%left '+' '-'
%left '*' '/' '%'
%right '!' '~' NEG POINTERSTAR
%nonassoc '->' '.' SQUAREBRACKET

%%
{- Program -}
Program : Fdecl {$1}
        | Typedef {$1}
        | Sdecl {$1}
        | Sdef {$1}


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

Sdecl : struct ident ';' {Sdecl $2}

Sdef : struct ident '{' Fieldlist '}' ';' {Sdef $2 $4}

Field : Type ident ';' {Field $1 $2}

Fieldlist : {- Empty -} {[]}
          | Field Fieldlist {$1 : $2}

{- CTypeIdent is for types which have been defined using typedef -}
Type  : int {CInt}
      | bool {CBool}
      | ident {CTypeIdent $1}
      | void {CVoid}
      | Type '*' {CPtr $1}
      | Type '[' ']' {CArray $1}
      | struct ident {CStruct $2}

{
parseError :: [Token] -> a
parseError t = error ("ParseOther Error: " ++ show t)

convertToLvalue :: Exp -> Lvalue
convertToLvalue (Var ident) = Variable ident
convertToLvalue (Dot lvalue ident) = LvalueDot (convertToLvalue lvalue) ident
convertToLvalue (Arrow lvalue ident) = LvalueArrow (convertToLvalue lvalue) ident
convertToLvalue (PointerStar lvalue) = LvaluePointerStar (convertToLvalue lvalue)
convertToLvalue (ArrayAccess lvalue expr) =
  LvalueArrayAccess (convertToLvalue lvalue) expr
convertToLvalue other = error("Not an lvalue:" ++ show other)

formAsgn :: Exp -> Asnop -> Exp -> Simp
formAsgn e1 op e2 =
  Asgn (convertToLvalue e1) op e2

formPost :: Exp -> Postop -> Simp
formPost e1 op =
  Post (convertToLvalue e1) op

checkDeclAsgn :: CType -> Ident -> Asnop -> Exp -> Decl
checkDeclAsgn t v op e =
  case op of 
    Equal -> DeclAsgn t v e
    _ -> error "Invalid assignment operator on a declaration"

checkDec n = if (n > 2^31) then error "Decimal too big" else Const n
checkHex n = if (n >= 2^32) then error "Hex too big" else Const n

}