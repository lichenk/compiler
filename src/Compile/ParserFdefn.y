{
module Compile.ParserFdefn where

import Compile.Lexer
import Compile.Types.Ops
import Compile.Types.FAST
}

%name parseFdefnTokens
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
  ident   {TokVarIdent $$}
  structIdent   {TokStructIdent $$}
  typeIdent   {TokTypeIdent $$}
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
        | Fdefn {$1}
        | Typedef {$1}
        | Sdecl {$1}
        | Sdef {$1}


{- Function declaration -}
Fdecl : Type ident Paramlist ';' {Fdecl $1 $2 $3}

{- Function definition -}
Fdefn : Type ident Paramlist Block {Fdefn $1 $2 $3 $4}

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

Sdecl : struct structIdent ';' {Sdecl $2}

Sdef : struct structIdent '{' Fieldlist '}' ';' {Sdef $2 $4}

Field : Type ident ';' {Field $1 $2}

Fieldlist : {- Empty -} {[]}
          | Field Fieldlist {$1 : $2}

Block : '{' Stmts '}' {$2}

{- CTypeIdent is for types which have been defined using typedef -}
Type  : int {CInt}
      | bool {CBool}
      | typeIdent {CTypeIdent $1}
      | void {CVoid}
      | Type '*' {CPtr $1}
      | Type '[' ']' {CArray $1}
      | struct structIdent {CStruct $2}

Decl  : Type ident asgnop Exp {checkDeclAsgn $1 $2 $3 $4}
      | Type ident {JustDecl $1 $2}

Stmts : {- Empty -} {[]}
      | Stmt Stmts  {$1 : $2}

Stmt  : Simp ';' {Simp $1}
      | Ctrl {Ctrl $1}
      | Block {Blk $1}

Simp  : Exp asgnop Exp {formAsgn $1 $2 $3}
      | Exp postop {formPost $1 $2}
      | Decl {Decl $1}
      | Exp {Exp $1}

Simpopt : {- Empty -} {Epsilon}
        | Simp {Simpopt $1}

Ctrl  : if '(' Exp ')' Stmt %prec THEN {If $3 $5 Eps}
      | if '(' Exp ')' Stmt else Stmt {If $3 $5 (Else $7)}
      | while '(' Exp ')' Stmt {While $3 $5}
      | for '(' Simpopt ';' Exp ';' Simpopt ')' Stmt {For $3 $5 $7 $9}
      | ret Exp ';' {Ret $2}
      | ret ';' {RetVoid}
      | assert '(' Exp ')' ';' {Assert $3}

{- Argument list -}
Arglistfollow : {- Empty -} {[]}
              | ',' Exp Arglistfollow {$2 : $3}

Arglist : '(' ')' {[]}
        | '(' Exp Arglistfollow ')' {$2 : $3}

Exp : '(' Exp ')' {$2}
    | Intconst {$1}
    | true {CTrue}
    | false {CFalse}
    | ident {Var $1}
    | NULL {NULL}
    | Operation {$1}
    | ident Arglist {Call $1 $2}
    | Exp '.' ident {Dot $1 $3}
    | Exp '->' ident {Arrow $1 $3}
    | alloc '(' Type ')' {Alloc $3}
    | '*' Exp %prec POINTERSTAR {PointerStar $2}
    | alloc_array '(' Type ',' Exp ')' {AllocArray $3 $5}
    | Exp '[' Exp ']' %prec SQUAREBRACKET {ArrayAccess $1 $3}

Operation : Exp '-' Exp {Binary Sub $1 $3}
          | Exp '+' Exp {Binary Add $1 $3}
          | Exp '*' Exp {Binary Mul $1 $3}
          | Exp '/' Exp {Binary Div $1 $3}
          | Exp '%' Exp {Binary Mod $1 $3}
          | Exp '<<' Exp {Binary ShiftL $1 $3}
          | Exp '>>' Exp {Binary ShiftR $1 $3}
          | Exp '<' Exp {Binary Less $1 $3}
          | Exp '>' Exp {Binary Greater $1 $3}
          | Exp '<=' Exp {Binary Leq $1 $3}
          | Exp '>=' Exp {Binary Geq $1 $3}
          | Exp '!=' Exp {Binary Neq $1 $3}
          | Exp '&' Exp {Binary BAnd $1 $3}
          | Exp '^' Exp {Binary BXor $1 $3}
          | Exp '|' Exp {Binary BOr $1 $3}
          | Exp '&&' Exp {Binary LAnd $1 $3}
          | Exp '||' Exp {Binary LOr $1 $3}
          | Exp '==' Exp {Binary Eq $1 $3}
          | '!' Exp {Unary Not $2}
          | '~' Exp {Unary Flip $2}
          | '-' Exp %prec NEG {Unary Neg $2}
          | Exp '?' Exp ':' Exp {Ternary $1 $3 $5}

Intconst  : dec {checkDec $1}
          | hex {checkHex $1}

{
parseError :: [Token] -> a
parseError t = error ("ParseFdefn Error: " ++ show t)

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