module Compile.UnLazifyTast where
import Compile.Types.TAST

unlazifyTast tast = all unlazifyGdecl tast

unlazifyGdecl :: Gdecl -> Bool
unlazifyGdecl (Fdecl ctype ident paramlist) = (unlazifyType ctype) && (unlazifyIdent ident ) && ( unlazifyParamList paramlist )
unlazifyGdecl (Fdefn ctype ident paramlist block) = (unlazifyType ctype ) && (unlazifyIdent ident) && ( unlazifyParamList paramlist ) && (unlazifyBlk block)
unlazifyGdecl (Typedef ctype ident) = ( unlazifyType ctype ) && ( unlazifyIdent ident )
unlazifyGdecl (Sdecl ident) = ( unlazifyIdent ident )
unlazifyGdecl (Sdef ident fieldlist) = ( unlazifyIdent ident ) && (all unlazifyField fieldlist )

unlazifyBlk :: [Stmt] -> Bool
unlazifyBlk stmts = all unlazifyStmt stmts

unlazifyField :: Field -> Bool
unlazifyField (Field ctype ident) = (unlazifyType ctype ) && (unlazifyIdent ident)

unlazifyParamList paramlist = all unlazifyParam paramlist

unlazifyParam (Param ctype ident) = (unlazifyType ctype ) && ( unlazifyIdent ident)

unlazifyStmt (Ctrl c) = (unlazifyCtrl c)
unlazifyStmt (Simp simp) = (unlazifySimp simp)
unlazifyStmt (Blk stmts) = all unlazifyStmt stmts


unlazifySimp (Asgn lval asnop expr) = ((unlazifyLvalue lval) ) && (unlazifyExp expr)
unlazifySimp (Decl ctype v) = (unlazifyType ctype)
unlazifySimp (Exp expr) = (unlazifyExp expr)

unlazifyType :: CType -> Bool
unlazifyType CInt = True
unlazifyType CBool = True
unlazifyType CVoid = True
unlazifyType (CTypeIdent ident) = True
unlazifyType (CPtr ctype) = (unlazifyType ctype)
unlazifyType (CArray ctype) = (unlazifyType ctype)
unlazifyType (CStruct ident) = True
unlazifyType (CAny) = True
unlazifyType CNoType = False

unlazifyLvalue :: Lvalue -> Bool
unlazifyLvalue (Variable x ltype) = unlazifyType ltype
unlazifyLvalue (LvalueDot lvalue ident ltype) = ( unlazifyLvalue lvalue )&& (unlazifyIdent ident) && ( unlazifyType ltype )
unlazifyLvalue (LvaluePointerStar lvalue ltype) = ( unlazifyLvalue lvalue ) && (unlazifyType ltype)
unlazifyLvalue (LvalueArrayAccess lvalue expr ltype) = ( unlazifyLvalue lvalue ) && ( unlazifyExp expr ) && ( unlazifyType ltype )

unlazifyCtrl :: Ctrl -> Bool
unlazifyCtrl (If exp1 stmt elseopt) = (unlazifyExp exp1) &&(unlazifyStmt stmt)&& (unlazifyStmt elseopt)
unlazifyCtrl (While exp1 stmt) = (unlazifyExp exp1) &&(unlazifyStmt stmt)
unlazifyCtrl (Ret exp1) = (unlazifyExp exp1)
unlazifyCtrl RetVoid = True

unlazifyExp :: Exp -> Bool
unlazifyExp (Const x exprType) =  ( unlazifyType exprType )
unlazifyExp (CTrue exprType) =  ( unlazifyType exprType )
unlazifyExp (CFalse exprType) =  ( unlazifyType exprType )
unlazifyExp (Var x exprType) =  ( unlazifyType exprType )
unlazifyExp (Unary op expr exprType) =   (unlazifyExp expr) && ( unlazifyType exprType )
unlazifyExp (Binary binop exp1 exp2 exprType) =   ( (unlazifyExp exp1) ) && ( (unlazifyExp exp2) ) && ( unlazifyType exprType )
unlazifyExp (Ternary exp1 exp2 exp3 exprType) =   ( (unlazifyExp exp1) ) && ( (unlazifyExp exp2) ) && ( (unlazifyExp exp3) ) && ( unlazifyType exprType )
unlazifyExp (Call f args exprType) =  (all unlazifyExp args) && ( unlazifyType exprType )
unlazifyExp (Dot expr ident exprType) =  ( unlazifyExp expr ) && ( unlazifyType exprType )
unlazifyExp (Alloc ctype exprType) =  ( unlazifyType ctype ) && ( unlazifyType exprType )
unlazifyExp (PointerStar expr exprType) =  ( unlazifyExp expr ) && ( unlazifyType exprType )
unlazifyExp (AllocArray ctype expr exprType) =  ( unlazifyType ctype ) && ( unlazifyExp expr ) && ( unlazifyType exprType )
unlazifyExp (ArrayAccess expr1 expr2 exprType) =  ( unlazifyExp expr1 ) && ( unlazifyExp expr2 ) && ( unlazifyType exprType )
unlazifyExp (NULL exprType) =  ( unlazifyType exprType )

unlazifyIdent _ = True
