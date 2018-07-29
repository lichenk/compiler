module Compile.UnfuckStruct where
import Compile.Types.TAST

unfuckTast tast = map unfuckGdecl tast

unfuckGdecl :: Gdecl -> Gdecl
unfuckGdecl (Fdecl ctype ident paramlist) = Fdecl ctype ident paramlist
unfuckGdecl (Fdefn ctype ident paramlist block) = Fdefn ctype ident  (unfuckParamList paramlist)  (unfuckBlk block)
unfuckGdecl (Typedef ctype ident) = Typedef ctype ident
unfuckGdecl (Sdecl ident) = Sdecl ident
unfuckGdecl (Sdef ident fieldlist) = Sdef ident fieldlist

unfuckBlk :: [Stmt] -> [Stmt]
unfuckBlk stmts = map unfuckStmt stmts

unfuckField :: Field -> Field
unfuckField (Field ctype ident) = Field (ctype )  (unfuckIdent ident)

unfuckParamList paramlist = map unfuckParam paramlist

unfuckParam (Param ctype ident) = Param (unfuckType ctype )  ( unfuckIdent ident)

unfuckStmt (Ctrl c) = Ctrl (unfuckCtrl c)
unfuckStmt (Simp simp) = Simp (unfuckSimp simp)
unfuckStmt (Blk stmts) = Blk (map unfuckStmt stmts)


unfuckSimp (Asgn lval asnop expr) = Asgn ((unfuckLvalue lval) ) asnop  (unfuckExp expr)
unfuckSimp (Decl ctype v) = Decl (unfuckType ctype) v
unfuckSimp (Exp expr) = Exp (unfuckExp expr)

unfuckType :: CType -> CType
unfuckType (CPtr (CStruct ident)) = CStruct ident -- Unfuck is done over here!
unfuckType CInt = CInt
unfuckType CBool = CBool
unfuckType CVoid = CVoid
unfuckType (CTypeIdent ident) = CTypeIdent ident
unfuckType (CPtr ctype) = CPtr ctype -- Unfuck these!
unfuckType (CArray ctype) = CArray ctype -- Do not unfuck these!
unfuckType (CStruct ident) = CStruct ident -- CStruct ident
unfuckType (CAny) = CAny
unfuckType CNoType = CNoType

unfuckCtrl :: Ctrl -> Ctrl
unfuckCtrl (If exp1 stmt elseopt) = If (unfuckExp exp1) (unfuckStmt stmt) (unfuckStmt elseopt)
unfuckCtrl (While exp1 stmt) = While (unfuckExp exp1) (unfuckStmt stmt)
unfuckCtrl (Ret exp1) = Ret (unfuckExp exp1)
unfuckCtrl RetVoid = RetVoid

unfuckLvalue :: Lvalue -> Lvalue
unfuckLvalue (LvalueDot expin@(LvaluePointerStar expr exprType1) ident exprType2) =
	case isNextLvalueDot expr of
		True ->  LvalueDot (unfuckLvalue expin) ident exprType2
		False -> LvalueDot (modifyLvalueType exprType1 (unfuckLvalue expr)) ident exprType2
unfuckLvalue (Variable x ltype) = Variable x (unfuckType ltype)
unfuckLvalue (LvalueDot lvalue ident ltype) = LvalueDot ( unfuckLvalue lvalue ) (unfuckIdent ident)  ltype
unfuckLvalue (LvalueArrayAccess lvalue expr ltype) = LvalueArrayAccess( unfuckLvalue lvalue )  ( unfuckExp expr ) ltype
unfuckLvalue (LvaluePointerStar lvalue ltype) = LvaluePointerStar( unfuckLvalue lvalue )  (unfuckType ltype)

unfuckExp :: Exp -> Exp
unfuckExp (Dot expin@(PointerStar expr exprType1) ident exprType2) =
	case isNextDot expr of
		True ->  Dot (unfuckExp expin) ident exprType2
		False -> Dot (modifyExpType exprType1 (unfuckExp expr)) ident exprType2
unfuckExp (Dot expr ident exprType) =  (Dot (unfuckExp expr) ident exprType)
unfuckExp (Const x exprType) =  Const x ( unfuckType exprType )
unfuckExp (CTrue exprType) =  CTrue ( unfuckType exprType )
unfuckExp (CFalse exprType) =  CFalse ( unfuckType exprType )
unfuckExp (Var x exprType) =  Var x ( unfuckType exprType )
unfuckExp (Unary op expr exprType) =   Unary op (unfuckExp expr)  ( unfuckType exprType )
unfuckExp (Binary binop exp1 exp2 exprType) =   Binary binop ( (unfuckExp exp1) )  ( (unfuckExp exp2) )  ( unfuckType exprType )
unfuckExp (Ternary exp1 exp2 exp3 exprType) = 
	let
		exp1new = unfuckExp exp1
		exp2new = unfuckExp exp2
		exp3new = unfuckExp exp3
	in
		case exp1new of
			CTrue  _ -> exp2new
			CFalse _ -> exp3new
			_ -> Ternary exp1new exp2new exp3new (unfuckType exprType)
unfuckExp (Call f args exprType) =  Call f (map unfuckExp args)  ( unfuckType exprType )
unfuckExp (Alloc ctype exprType) =  Alloc ctype  ( unfuckType exprType )
unfuckExp (PointerStar expr exprType) = PointerStar ( unfuckExp expr )  ( unfuckType exprType )
unfuckExp (AllocArray ctype expr exprType) = AllocArray ctype  ( unfuckExp expr )  exprType
unfuckExp (ArrayAccess expr1 expr2 exprType) = ArrayAccess ( unfuckExp expr1 )  ( unfuckExp expr2 ) exprType
unfuckExp (NULL exprType) = NULL ( unfuckType exprType )

isNextLvalueDot :: Lvalue -> Bool
isNextLvalueDot (LvalueDot _ _ _) = True
isNextLvalueDot _ = False


isNextDot :: Exp -> Bool
isNextDot (Dot _ _ _) = True
isNextDot _ = False


modifyExpType :: CType -> Exp -> Exp
modifyExpType ctype (Const n _) = (Const n ctype)
modifyExpType ctype (CTrue  _) = (CTrue  ctype)
modifyExpType ctype (CFalse  _) = (CFalse  ctype)
modifyExpType ctype (Var ident  _) = (Var ident  ctype)
modifyExpType ctype (Unary unop exp1  _) = (Unary unop exp1  ctype)
modifyExpType ctype (Binary binop exp1 exp2  _) = (Binary binop exp1 exp2  ctype)
modifyExpType ctype (Ternary exp1 exp2 exp3 _) = (Ternary exp1 exp2 exp3 ctype)
modifyExpType ctype (Call ident l _) = (Call ident l ctype)
modifyExpType ctype (Dot exp1 ident _) = (Dot exp1 ident ctype)
modifyExpType ctype (Alloc ctype2 _) = (Alloc ctype2 ctype)
modifyExpType ctype (PointerStar exp1 _) = (PointerStar exp1 ctype)
modifyExpType ctype (AllocArray ctype2 exp1 _) = (AllocArray ctype2 exp1 ctype)
modifyExpType ctype (ArrayAccess exp1 exp2 _) = (ArrayAccess exp1 exp2 ctype)
modifyExpType ctype (NULL _) = (NULL ctype)

modifyLvalueType ctype (Variable ident _) = (Variable ident ctype)
modifyLvalueType ctype (LvalueDot lvalue ident _) = (LvalueDot lvalue ident ctype)
modifyLvalueType ctype (LvaluePointerStar lvalue _) = (LvaluePointerStar lvalue ctype)
modifyLvalueType ctype (LvalueArrayAccess lvalue exp1 _) = (LvalueArrayAccess lvalue exp1 ctype)

unfuckIdent ident = ident
