{- L1 Compiler
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>
                Rokhini Prabhu <rokhinip@andrew.cmu.edu>

   Defines the AST we parse to
-}
module Compile.Types.FAST where

import Numeric as Num
import Compile.Types.Ops

type Ident = String -- Identifier for a variable (variable name)

type AST = Gdecl

-- Global declaration is either a function declaration, function definition or typedef
data Gdecl = Fdecl CType Ident Paramlist
           | Fdefn CType Ident Paramlist Block
           | Typedef CType Ident
           | Sdecl Ident
           | Sdef Ident Fieldlist

type Fieldlist = [Field]

data Field = Field CType Ident

-- Parameter to function
data Param = Param CType Ident

-- Parameter list
type Paramlist = [Param]

type Block = [Stmt]
data CType = CInt
           | CBool
           | CTypeIdent Ident
           | CVoid
           | CPtr CType
           | CArray CType
           | CStruct Ident
           | CNoType deriving (Eq, Ord)
data Decl = JustDecl {typ :: CType, var :: Ident}     -- Declaring without instantiating
          | DeclAsgn {typ :: CType, var :: Ident, e :: Exp}     -- Declaring with instantiation
-- Block replaces stmts in this
data Stmt = Simp Simp
          | Ctrl Ctrl
          | Blk Block
data Simp = Asgn Lvalue Asnop Exp
          | Post Lvalue Postop
          | Decl Decl
          | Exp Exp
data Simpopt = Epsilon
             | Simpopt Simp
data Lvalue = Variable Ident
            | LvalueDot Lvalue Ident
            | LvalueArrow Lvalue Ident
            | LvaluePointerStar Lvalue
            | LvalueArrayAccess Lvalue Exp
data Elseopt = Eps
             | Else Stmt
data Ctrl = If Exp Stmt Elseopt
          | While Exp Stmt
          | For Simpopt Exp Simpopt Stmt
          | Ret Exp
          | RetVoid
          | Assert Exp
data Exp = Const Int
         | CTrue 
         | CFalse 
         | Var Ident 
         | Unary Unop Exp 
         | Binary Binop Exp Exp 
         | Ternary Exp Exp Exp
         | Call Ident [Exp] -- Function call. Call(ident,[exp]), where ident is function name, [exp] is argument list
         | Dot Exp Ident
         | Arrow Exp Ident
         | Alloc CType
         | PointerStar Exp
         | AllocArray CType Exp
         | ArrayAccess Exp Exp
         | NULL

-- Note to the student: You will probably want to write a new pretty printer
-- using the module Text.PrettyPrint.HughesPJ from the pretty package
-- This is a quick and dirty pretty printer.
-- Once that is written, you may find it helpful for debugging to switch
-- back to the deriving Show instances.

instance Show Gdecl where
  show (Fdecl ctype ident paramlist) = show ctype ++ " " ++ show ident ++ show paramlist ++ "\n"
  show (Fdefn ctype ident paramlist block) = show ctype ++ " " ++ show ident ++ show paramlist ++ "\n" ++ show block ++ "\n"
  show (Typedef ctype ident) = "typedef " ++ show ctype ++ " " ++ show ident ++ "\n"
  show (Sdecl ident) = "struct " ++ show ident ++ ";'"
  show (Sdef ident fieldlist) = "struct " ++ show ident ++ show fieldlist ++ ";"

instance Show Field where
  show (Field ctype ident) = show ctype ++ " " ++ ident ++ ", "

instance Show Param where
  show (Param ctype ident) = show ctype ++ " " ++ show ident

instance Show Stmt where
  show (Ctrl c) = show c ++ "\n"
  show (Simp simp) = show simp ++ ";\n"
  show (Blk stmts) = "{\n" ++ (unlines $ (map (\stmt ->"\t" ++ show stmt) stmts)) ++ "}\n"

instance Show Simp where
  show (Asgn lval asnop exp) = (show lval) ++ " " ++ (show asnop) ++ " " ++ (show exp)
  show (Post lval postop) = (show lval) ++ (show postop)
  show (Decl d) = show d
  show (Exp e) = (show e)

instance Show Decl where
  show (JustDecl t i) = (show t) ++ " " ++ i
  show (DeclAsgn t x e) = (show t) ++ " " ++ x ++ " = " ++ (show e)

instance Show CType where
  show CInt = "int"
  show CBool = "bool"
  show CVoid = "void"
  show (CTypeIdent ident) = ident
  show (CPtr ctype) = show ctype ++ "*"
  show (CArray ctype) = show ctype ++ "[]"
  show (CStruct ident) = "struct " ++ ident
  show CNoType = "NOTYPE"

instance Show Lvalue where
  show (Variable x) = x
  show (LvalueDot lvalue ident) = show lvalue ++ "." ++ ident
  show (LvalueArrow lvalue ident) = show lvalue ++ "->" ++ ident
  show (LvaluePointerStar lvalue) = "*" ++ show lvalue
  show (LvalueArrayAccess lvalue expr) = show lvalue ++ "[" ++ show expr ++ "]"

instance Show Ctrl where
  show (If exp1 stmt elseopt) = "if (" ++ (show exp1) ++ ")\n" ++ (show stmt) ++ (show elseopt)
  show (While exp1 stmt) = "while (" ++ (show exp1) ++ ")\n" ++ (show stmt)
  show (For simp1 exp1 simp2 stmt) = "For (" ++ (show simp1) ++ "; " ++ (show exp1) ++ "; " ++ (show simp2) ++ ")\n" ++ (show stmt)
  show (Ret exp1) = "return " ++ (show exp1) ++ ";"
  show (RetVoid) = "return;"
  show (Assert exp1) = "assert(" ++ (show exp1) ++ ");"

instance Show Elseopt where
  show Eps = "\n"
  show (Else stmt) = "else\n" ++ (show stmt)

instance Show Simpopt where
  show Epsilon = ";"
  show (Simpopt simp) = (show simp)   

instance Show Exp where
  show (Const x) = show x
  show (CTrue) = "true"
  show (CFalse) = "false"
  show (Var x) = x
  show (Unary op e) = "(" ++ (show op) ++ (show e) ++ ")"
  show (Binary binop exp1 exp2) = "(" ++ (show exp1) ++ " " ++ (show binop) ++ " " ++ (show exp2) ++ ")"
  show (Ternary exp1 exp2 exp3) = "(" ++ (show exp1) ++ " ? " ++ (show exp2) ++ " : " ++ (show exp3) ++ ")"
  show (Call ident arglist) = show ident ++ "(" ++ (concat (map (\arg -> show arg ++ ",") arglist)) ++ ")"
  show (Dot expr ident) = show expr ++ "." ++ ident
  show (Arrow expr ident) = show expr ++ "->" ++ ident
  show (Alloc ctype) = "alloc(" ++ show ctype ++ ")"
  show (PointerStar expr) = "*" ++ show expr
  show (AllocArray ctype expr) = "alloc_array(" ++ show ctype ++ "," ++ show expr ++ ")"
  show (ArrayAccess expr1 expr2) = show expr1 ++ "[" ++ show expr2 ++ "]"
  show (NULL) = "NULL"

