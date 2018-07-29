module Compile.Types.PTree where

import Text.ParserCombinators.Parsec.Pos (SourcePos)
import Numeric as Num
import Compile.Types.Ops

-- Anywhere that keywords for C0 may interfere with Haskell keywords, I prefix "C"

type Ident = String -- Identifier for a variable (variable name)

data Program = Func Block SourcePos
type Block = [Stmt]
data CType = CInt | CBool
data Decl = JustDecl {typ :: CType, var :: Ident, pos :: SourcePos}     -- Declaring without instantiating
          | DeclAsgn {typ :: CType, var :: Ident, e :: Exp, pos :: SourcePos}     -- Declaring with instantiation
-- Block replaces stmts in this
data Stmt = Simp Simp
          | Ctrl Ctrl
          | Blk Block
data Simp = Asgn Lvalue Asnop Exp SourcePos
          | Post Lvalue Postop SourcePos
          | Decl SourcePos
          | Exp SourcePos
data Simpopt = Epsilon
             | Simp Simp
data Lvalue = Var Ident
            | Parens Lvalue
data Elseopt = Epsilon
             | Else Stmt
data Ctrl = If Exp Stmt Elseopt
          | While Exp Stmt
          | For Simpopt Exp Simpopt Stmt
          | Ret Exp
data Exp = Parens Exp SourcePos
         | Num Integer SourcePos
         | CTrue SourcePos
         | CFalse SourcePos
         | Var Ident SourcePos
         | Unary Unop Exp SourcePos
         | Binary Exp Binop Exp SourcePos
         | Ternary Exp Exp Exp SourcePos