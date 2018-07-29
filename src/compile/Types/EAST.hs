module Compile.Types.EAST where -- Elaborated Abstract Syntax Tree

import Data.List
import Numeric as Numeric
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
           | CAny -- Reserved for NULL. Note that a NULL is a CPtr CAny
           | CNoType deriving (Eq, Ord)

data Stmt = Simp Simp
          | Ctrl Ctrl
          | Blk Block
data Simp = Asgn Lvalue Asnop Exp
          | Decl {typ :: CType, var :: Ident}
          | Exp Exp
data Lvalue = Variable Ident
            | LvalueDot Lvalue Ident
            | LvaluePointerStar Lvalue
            | LvalueArrayAccess Lvalue Exp
data Ctrl = If Exp Stmt Stmt
          | While Exp Stmt
          | Ret Exp
          | RetVoid
data Exp = Const Int
         | CTrue 
         | CFalse 
         | Var Ident 
         | Unary Unop Exp 
         | Binary Binop Exp Exp 
         | Ternary Exp Exp Exp
         | Call Ident [Exp]
         | Dot Exp Ident
         | Alloc CType
         | PointerStar Exp
         | AllocArray CType Exp
         | ArrayAccess Exp Exp
         | NULL

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
  show (Ctrl c) = show c 
  show (Simp simp) = show simp ++ ";"
  show (Blk stmts) = "{\n" ++ (unlines $ (map (\stmt ->"\t" ++ show stmt) stmts)) ++ "}\n"

instance Show Simp where
  show (Asgn lval asnop exp) = (show lval) ++ " " ++ (show asnop) ++ " " ++ (show exp)
  show (Decl t v) = (show t) ++ " " ++ v
  show (Exp e) = (show e)

instance Show CType where
  show CInt = "int"
  show CBool = "bool"
  show CVoid = "void"
  show (CTypeIdent ident) = ident
  show (CPtr ctype) = show ctype ++ "*"
  show (CArray ctype) = show ctype ++ "[]"
  show (CStruct ident) = "struct " ++ ident
  show (CAny) = "CAny"
  show CNoType = "NOTYPE"

instance Show Lvalue where
  show (Variable x) = x
  show (LvalueDot lvalue ident) = show lvalue ++ "." ++ ident
  show (LvaluePointerStar lvalue) = "*" ++ show lvalue
  show (LvalueArrayAccess lvalue expr) = show lvalue ++ "[" ++ show expr ++ "]"

instance Show Ctrl where
  show (If exp1 stmt elseopt) = "if (" ++ (show exp1) ++ ")\n" ++ (show stmt) ++ (show elseopt)
  show (While exp1 stmt) = "while (" ++ (show exp1) ++ ")\n" ++ (show stmt)
  show (Ret exp1) = "return " ++ (show exp1) ++ ";"
  show RetVoid = "return;"

instance Show Exp where
  show (Const x) = show x
  show (CTrue) = "true"
  show (CFalse) = "false"
  show (Var x) = x
  show (Unary op e) = "(" ++ (show op) ++ (show e) ++ ")"
  show (Binary binop exp1 exp2) = "(" ++ (show exp1) ++ " " ++ (show binop) ++ " " ++ (show exp2) ++ ")"
  show (Ternary exp1 exp2 exp3) = "(" ++ (show exp1) ++ " ? " ++ (show exp2) ++ " : " ++ (show exp3) ++ ")"
  show (Call f args) = f ++ "(" ++ (intercalate ", " (map (show) args)) ++ ")"
  show (Dot expr ident) = show expr ++ "." ++ ident
  show (Alloc ctype) = "alloc(" ++ show ctype ++ ")"
  show (PointerStar expr) = "*" ++ show expr
  show (AllocArray ctype expr) = "alloc_array(" ++ show ctype ++ "," ++ show expr ++ ")"
  show (ArrayAccess expr1 expr2) = show expr1 ++ "[" ++ show expr2 ++ "]"
  show (NULL) = "NULL"

