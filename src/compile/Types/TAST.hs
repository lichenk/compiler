module Compile.Types.TAST where -- Elaborated Abstract Syntax Tree here

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
           | CRawArray CType
           | CStruct Ident
           | CAny -- Reserved for NULL. Note that a NULL is a CPtr CAny
           | CEightByte
           | CInt8
           | CNoType deriving (Eq, Ord)

data Stmt = Simp Simp
          | Ctrl Ctrl
          | Blk Block
data Simp = Asgn Lvalue Asnop Exp
          | Decl {typ :: CType, var :: Ident}
          | Exp Exp
data Lvalue = Variable Ident CType
            | LvalueDot Lvalue Ident CType
            | LvaluePointerStar Lvalue CType
            | LvalueArrayAccess Lvalue Exp CType
data Ctrl = If Exp Stmt Stmt
          | While Exp Stmt
          | Ret Exp
          | RetVoid
data Exp = Const Int CType
         | CTrue  CType
         | CFalse  CType
         | Var Ident  CType
         | Unary Unop Exp  CType
         | Binary Binop Exp Exp  CType
         | Ternary Exp Exp Exp CType
         | Call Ident [Exp] CType
         | Dot Exp Ident CType
         | Alloc CType CType
         | PointerStar Exp CType
         | AllocArray CType Exp CType
         | ArrayAccess Exp Exp CType
         | NULL CType

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
  show (Simp simp) = show simp ++ ";\n"
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
  show (CRawArray ctype) = show ctype ++ "[RAW]"
  show (CStruct ident) = "struct " ++ ident
  show (CAny) = "CAny"
  show CNoType = "NOTYPE"


instance Show Lvalue where
  show (Variable x ltype) = "(" ++ x ++ ":: " ++ show ltype ++ ")"
  show (LvalueDot lvalue ident ltype) = "(" ++ show lvalue ++ "." ++ ident ++ ":: " ++ show ltype ++ ")"
  show (LvaluePointerStar lvalue ltype) = "(" ++ "*" ++ show lvalue ++ ":: " ++ show ltype ++ ")"
  show (LvalueArrayAccess lvalue expr ltype) = "(" ++ show lvalue ++ "[" ++ show expr ++ "]" ++ ":: " ++ show ltype ++ ")"

instance Show Ctrl where
  show (If exp1 stmt elseopt) = "if (" ++ (show exp1) ++ ")\n" ++ (show stmt) ++ (show elseopt)
  show (While exp1 stmt) = "while (" ++ (show exp1) ++ ")\n" ++ (show stmt)
  show (Ret exp1) = "return " ++ (show exp1) ++ ";"
  show RetVoid = "return;"

instance Show Exp where
  show (Const x exprType) = "(" ++ show x ++ "::" ++ show exprType ++ ")"
  show (CTrue exprType) = "(" ++ "true" ++ "::" ++ show exprType ++ ")"
  show (CFalse exprType) = "(" ++ "false" ++ "::" ++ show exprType ++ ")"
  show (Var x exprType) = "(" ++ x ++ "::" ++ show exprType ++ ")"
  show (Unary op e exprType) = "(" ++ "(" ++ (show op) ++ (show e) ++ ")" ++ "::" ++ show exprType ++ ")"
  show (Binary binop exp1 exp2 exprType) = "(" ++ "(" ++ (show exp1) ++ " " ++ (show binop) ++ " " ++ (show exp2) ++ ")" ++ "::" ++ show exprType ++ ")"
  show (Ternary exp1 exp2 exp3 exprType) = "(" ++ "(" ++ (show exp1) ++ " ? " ++ (show exp2) ++ " : " ++ (show exp3) ++ ")" ++ "::" ++ show exprType ++ ")"
  show (Call f args exprType) = "(" ++ f ++ "(" ++ (intercalate ", " (map (show) args)) ++ ")" ++ "::" ++ show exprType ++ ")"
  show (Dot expr ident exprType) = "(" ++ show expr ++ "." ++ ident ++ "::" ++ show exprType ++ ")"
  show (Alloc ctype exprType) = "(" ++ "alloc(" ++ show ctype ++ ")" ++ "::" ++ show exprType ++ ")"
  show (PointerStar expr exprType) = "(" ++ "*" ++ show expr ++ "::" ++ show exprType ++ ")"
  show (AllocArray ctype expr exprType) = "(" ++ "alloc_array(" ++ show ctype ++ "," ++ show expr ++ ")" ++ "::" ++ show exprType ++ ")"
  show (ArrayAccess expr1 expr2 exprType) = "(" ++ show expr1 ++ "[" ++ show expr2 ++ "]" ++ "::" ++ show exprType ++ ")"
  show (NULL exprType) = "(" ++ "NULL" ++ "::" ++ show exprType ++ ")"


