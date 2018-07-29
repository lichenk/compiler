module Compile.SafeCodeGen where

import Compile.Constants
import Compile.Types
import qualified Compile.MapWrap as MapWrap
import qualified Data.Map as Map
import Compile.RegisterList
import Data.Tuple
import Debug.Trace

-- The first maps variables to their temps, the second is
-- the current number of temps, the third is field offset lookup for structs,
-- the fourth is a sizeof operator
type Alloc = (Map.Map String Int, Int, (CType -> String -> Int), (CType -> Int))

-- Get the decls from   
getDecls :: [Stmt] -> [Ident]
getDecls stmts = concatMap getDeclsStmt stmts

getDeclsStmt :: Stmt -> [Ident]
getDeclsStmt (Simp (Decl t v)) = [v]
getDeclsStmt (Simp _) = []
getDeclsStmt (Ctrl ctrl) = getDeclsCtrl ctrl
getDeclsStmt (Blk block) = getDecls block

getDeclsCtrl :: Ctrl -> [Ident]
getDeclsCtrl (If _ s1 s2) = getDeclsStmt s1 ++ getDeclsStmt s2
getDeclsCtrl (While e s) = getDeclsStmt s
getDeclsCtrl (Ret _) = []
getDeclsCtrl (RetVoid) = []

colorInArg :: Int -> CType -> ALoc
colorInArg n t =
  case n of
    0 -> AReg 5 t--edi
    1 -> AReg 4 t--esi
    2 -> AReg 3 t--edx
    3 -> AReg 2 t--ecx
    4 -> AReg 7 t--r8
    5 -> AReg 8 t--r9
    m -> AInArg (m) t

moveArgToTemp :: Alloc -> ((CType, Ident),Int) -> AAsm
moveArgToTemp (allocmap, _, _, _) ((t, arg), index) =
  case Map.member arg allocmap of
    True -> AAsm [ATemp ((Map.!) allocmap arg) t] ANopDontPropagate [ALoc (colorInArg index t)]
    False -> error("Bug in moveArgToTemp: arg not found in map")

moveArgsToTemps :: Alloc -> [(CType, Ident)] -> [AAsm]
moveArgsToTemps alloc arglist =
  let
    arglistWithIndex = zip arglist [0..]
  in
    map (moveArgToTemp alloc) arglistWithIndex

-- Really only going to work for functions.
-- Make sure you filter out the other stuff!
safeCodeGen :: (CType -> Int, CType -> String -> Int) -> AST -> [AAsm]
safeCodeGen (sizeof, fields) (Fdefn t n params stmts) = 
  let
    arglist = map (\(Param t i) -> (t, i)) params
    decls = (getDecls stmts) ++ (map snd arglist)
    temps = Map.fromList $ zip decls [0..]
    alloc = (temps, (length decls), fields, sizeof)
    moveArgsToTempsAsm = moveArgsToTemps alloc arglist
  in moveArgsToTempsAsm ++ (genBlock alloc stmts)

genBlock :: Alloc -> Block -> [AAsm]
genBlock alloc blk = concatMap (genStmt alloc) blk

genStmt :: Alloc -> Stmt -> [AAsm]
genStmt alloc (Simp simp) = genSimp alloc simp
genStmt alloc (Ctrl ctrl) = genCtrl alloc ctrl
genStmt alloc (Blk block) = genBlock alloc block

genSimp :: Alloc -> Simp -> [AAsm]
genSimp alloc@(allocmap, n, f, s) (Asgn lval op expr) = genAsgn alloc op lval expr
genSimp alloc (Decl _ _) = []
genSimp (allocmap, n, f, s) (Exp expr) = genExp (allocmap, n + 1, f, s) expr (ATemp  n (getExpType expr))

-- In the case of variables, returns the temp to be written to, otherwise returns the memory address to write to.
-- May use the current temp to write to, so increment the current temp number (n)
genLval :: Alloc -> Lvalue -> ([AAsm], ALoc)
genLval (allocmap, _, _, _) (Variable ident t) = ([], ATemp (allocmap Map.! ident) t)
genLval (allocmap, n, f, s) l@(LvalueDot lval field t) = 
  let
    structType = getLvalType lval
    (buildStruct, parentStruct) = evalLvalAsExp (allocmap, n+1, f, s) lval
    offset = [AAsm [(ATemp n (CPtr t))] AAdd [(ALoc parentStruct), (AImm (f structType field))]]
  in
    (buildStruct ++ offset, derefLoc (ATemp n (CPtr t)))
genLval (allocmap, n, f, s) (LvaluePointerStar lval t) =
  let
    (buildPtr, ptr) = evalLvalAsExp (allocmap, n, f, s) lval
  in
    (buildPtr, derefLoc ptr)
genLval (allocmap, n, f, s) (LvalueArrayAccess lval expr t) =
  let
    arrType = 
      case t of
        CStruct _ -> t
        _ -> CPtr t
    (buildArr, arr) = evalLvalAsExp (allocmap, n+1, f, s) lval
    idx = genExp (allocmap, n+3, f, s) expr (ATemp (n+2) CInt)
    arrLenAddr = [AAsm [ATemp (n+3) (CPtr CInt)] ASub [ALoc arr, AImm 8]]
    arrLen = [AAsm [ATemp (n+4) CInt] ANop [ALoc (derefLoc (ATemp (n+3) (CPtr CInt)))]]
    boundsBool = [AAsm [ATemp (n+5) CBool] ALeq [ALoc (ATemp (n+4) CInt), ALoc (ATemp (n+2) CInt)]]
    -- On the next line, idx is treated as a CPtr.  This is intentional
    offset = [AAsm [ATemp (n+3) arrType] AMul [ALoc (ATemp (n+2) arrType), AImm (s t)]]
    memloc = [AAsm [ATemp n arrType] AAdd [ALoc arr, ALoc (ATemp (n+3) arrType)]]
  in
    -- (buildArr ++ idx ++ arrLenAddr ++ arrLen ++ boundsBool ++ checkBool ++ offset ++ memloc, derefLoc (ATemp n (CPtr t)))
    (buildArr ++ idx ++ offset ++ memloc, derefLoc (ATemp n (CPtr t)))

-- Does exactly what it says it does.  Evaluates an lval exactly as if it were an expression instead.  Always 
-- returns a temp
evalLvalAsExp :: Alloc -> Lvalue -> ([AAsm], ALoc)
evalLvalAsExp (allocmap, n, f, s) (Variable ident t) = ([], ATemp (allocmap Map.! ident) t)
evalLvalAsExp (allocmap, n, f, s) inast@(LvalueDot lval field t) =
  let
    structType = getLvalType lval
    (buildStruct, parentStruct) = evalLvalAsExp (allocmap, n+1, f, s) lval
    offset = [AAsm [(ATemp (n+2) structType)] AAdd [(ALoc parentStruct), (AImm (f structType field))]]
    moveToDest =
      case t of
        -- Yo! This is a hack!
        (CPtr (CStruct _)) ->
          [AAsm [ATemp n t] ANop [ALoc (ATemp (n+2) structType)]]
        (CStruct _) ->
          [AAsm [ATemp n t] ANop [ALoc (ATemp (n+2) structType)]]
        _ ->
          [AAsm [ATemp n t] ANop [ALoc (derefLoc (ATemp (n+2) structType))]]
    ans = (buildStruct ++ offset ++ moveToDest, ATemp n t)
  in
    ans
evalLvalAsExp (allocmap, n, f, s) (LvaluePointerStar lval t) =
  let
    ptrType = getLvalType lval
    (buildPtr, ptrLoc) = evalLvalAsExp (allocmap, n+1, f, s) lval
    moveToDest =
      case ptrType of
        (CStruct _) -> [AAsm [ATemp n t] ANop [ALoc ptrLoc]]
        _ -> [AAsm [ATemp n t] ANop [ALoc (derefLoc ptrLoc)]]
  in
    (buildPtr ++ moveToDest, ATemp n t)
evalLvalAsExp (allocmap, n, f, s) (LvalueArrayAccess lval expr _) = 
  let
    (arrType,t) =
      case getLvalType lval of
        CArray (CStruct ctype) -> (CStruct ctype, CStruct ctype)
        CArray ctype -> (CPtr ctype, ctype)
    (buildArr, arr) = evalLvalAsExp (allocmap, n+1, f, s) lval
    idx = genExp (allocmap, n+3, f, s) expr (ATemp (n+2) CInt)
    arrLenAddr = [AAsm [ATemp (n+3) (CPtr CInt)] ASub [ALoc arr, AImm 8]]
    arrLen = [AAsm [ATemp (n+4) CInt] ANop [ALoc (derefLoc (ATemp (n+3) (CPtr CInt)))]]
    boundsBool = [AAsm [ATemp (n+5) CBool] ALeq [ALoc (ATemp (n+4) CInt), ALoc (ATemp (n+2) CInt)]]
    checkBool = [AIf (ACond (ALoc (ATemp (n+5) CBool))) [ACall (AReg 1 CInt) ("" ++ "raise") [(AImm 11)]] []]
    -- On the next line, idx is treated as a CPtr.  This is intentional
    offset = [AAsm [ATemp (n+3) arrType] AMul [ALoc (ATemp (n+2) arrType), AImm (s t)]]
    memloc = [AAsm [ATemp (n+4) arrType] AAdd [ALoc arr, ALoc (ATemp (n+3) arrType)]]
    deref = [AAsm [ATemp n t] ANop [ALoc (derefLoc (ATemp (n+4) arrType))]]
    retmem = [AAsm [ATemp n t] ANop [ALoc (ATemp (n+4) arrType)]]
  in
    case t of
      CStruct _ -> (buildArr ++ idx ++ offset ++ memloc ++ retmem, ATemp n t)
      _ -> (buildArr ++ idx ++ offset ++ memloc ++ deref, ATemp n t)

genAsgn :: Alloc -> Asnop -> Lvalue -> Exp -> [AAsm]
genAsgn alloc@(allocmap, n, f, s) Equal lval expr =
  let 
    (buildDest, destination) = genLval alloc lval
    buildExpr = genExp (allocmap, n+2, f, s) expr (ATemp (n+1) (getExpType expr))
  in
    buildDest ++ buildExpr ++ [AAsm [destination] ANop [ALoc (ATemp (n+1) (getExpType expr))]]
genAsgn alloc@(allocmap, n, f, s) (AsnOp bin) lval expr =
  let
    (buildDest, destination) = genLval alloc lval
    exprType = getExpType expr
    lvalType = getLvalType lval
    buildExpr = genExp (allocmap, n+2, f, s) expr (ATemp (n+1) exprType)
    derefDest = [AAsm [ATemp (n+2) lvalType] ANop [ALoc destination]]
    -- So now the expression's value is in n+1 and the LHS's existing value is in n+2
    -- Horrible hack so that we can reuse genExp for this.  Making temporary variables which hold what was computed above.
    tempmap = Map.union allocmap $ Map.fromList [("1GENASGNTEMP1", n+1), ("2GENASGNTEMP2", n+2)]
    tempExp = (Binary bin (Var "2GENASGNTEMP2" lvalType) (Var "1GENASGNTEMP1" exprType) lvalType)
    assign = genExp (tempmap, n+3, f, s) tempExp destination
  in
    buildDest ++ buildExpr ++ derefDest ++ assign

genExp :: Alloc -> Exp -> ALoc -> [AAsm]
genExp _ (Const n _) dest = [AAsm [dest] ANop [AImm $ fromIntegral n]]
genExp _ (CTrue _) dest = [AAsm [dest] ANop [AImm 1]]
genExp _ (CFalse _) dest = [AAsm [dest] ANop [AImm 0]]
genExp (allocmap, _, _, _) (Var var t) dest = 
  [AAsm [dest] ANop [ALoc $ ATemp (allocmap Map.! var) t]]
genExp (allocmap, n, f, s) (Unary unop exp _) dest = let
  cogen = genExp (allocmap, n + 1, f, s) exp (ATemp n (getExpType exp))
  assign = 
    case unop of
      Neg -> [AAsm [dest] ASub [AImm 0, ALoc $ ATemp n CInt]]
      Flip -> [AAsm [dest] ABXor [AImm (4294967295), ALoc $ ATemp n CInt]]
      Not -> [AAsm [dest] ASub [AImm 1, ALoc $ ATemp n CBool]]
  in cogen ++ assign
genExp alloc@(allocmap, n, f, s) (Binary binop exp1 exp2 _) dest = 
  case binop of
    LAnd -> genExp alloc (Ternary exp1 exp2 (CFalse CBool)(getExpType exp1)) dest
    LOr -> genExp alloc (Ternary exp1 (CTrue CBool) exp2 (getExpType exp1)) dest
    otherwise -> 
      let
        cogen1 = genExp (allocmap, n + 1, f, s) exp1 (ATemp n (getExpType exp1))
        cogen2 = genExp (allocmap, n + 2, f, s) exp2 (ATemp (n+1) (getExpType exp2))
        combine = [AAsm [dest] (genBinOp binop) [ALoc $ ATemp n CInt, ALoc $ ATemp (n + 1) CInt]]
      in cogen1 ++ cogen2 ++ combine
genExp (allocmap, n, f, s) (Ternary exp1 exp2 exp3 _) dest =
  let
    (cogen1, acond, nextNum) = genACond (allocmap, n, f, s) exp1
    cogen2 = genExp (allocmap, nextNum, f, s) exp2 dest
    cogen3 = genExp (allocmap, nextNum+1, f, s) exp3 dest
  in
    cogen1 ++ [AIf acond cogen2 cogen3]
genExp (allocmap, n, f, s) (Call func args _) dest =
  let
    argnums = [0.. ((length args) - 1)]
    argdests = zip args argnums
    exprs = map (\(expr, destNum) -> genExp (allocmap, n + destNum + 1, f, s) expr (ATemp (n + destNum) (getExpType expr))) argdests
    argtemplist = map (\(expr, destNum) -> (ALoc (ATemp (n + destNum) (getExpType expr)))) argdests
    m = n + (length args)
  in
    (concat exprs) ++ [(ACall dest (""++func) argtemplist)]
genExp (allocmap, n, fields, s) (Dot expr field t) dest =
  let
    struct = getExpType expr
    structgen :: [AAsm]
    structgen = genExpStructSpecial (allocmap, n + 1, fields, s) expr (ATemp n struct)
    offset = [AAsm [(ATemp (n+1) struct)] AAdd [(ALoc (ATemp n struct)), (AImm (fields struct field))]]
    moveToDest =
      case t of
        (CStruct _) ->
          [AAsm [dest] ANop [ALoc (ATemp (n+1) struct)]]
        _ ->
          [AAsm [dest] ANop [ALoc (derefLoc (ATemp (n+1) struct))]]
  in
    structgen ++ offset ++ moveToDest
genExp alloc@(allocmap, n, f, sizeof) (Alloc t _) dest = 
  let
    numBytes = sizeof t
  in
    genExp alloc (Call ("calloc") [Const numBytes CInt, Const 1 CInt] (CPtr t)) dest
genExp (allocmap, n, f, s) (PointerStar expr t) dest =
  let
    temp = genExp (allocmap, n+1, f, s) expr (ATemp n (getExpType expr))
    toDest = 
      case t of
        --CStruct _ -> [AAsm [dest] ANop [ALoc (ATemp n (getExpType expr))]]
        _ -> [AAsm [dest] ANop [ALoc (derefLoc (ATemp n (getExpType expr)))]]
  in
    temp ++ toDest
genExp (allocmap, n, f, sizeof) (AllocArray t expr _) dest =
  let
    numBytes = (sizeof t)
    findSize = genExp (allocmap, n+2, f, sizeof) expr (ATemp (n+1) CInt)
    totalSize = [AAsm [ATemp (n+2) (CPtr t)] AMul [ALoc (ATemp (n+1) CInt), AImm numBytes]]
    -- Now t(n+2) contains the size I want to pass to calloc
    -- I'll use the same horrible hack as way above to call calloc.  This makes sure that you aren't
    -- passing negatives to calloc.
    tempmap = Map.union allocmap $ Map.fromList [("1GENEXPTEMP1", (n+2))]
    tempexp = 
        (Call ("calloc") [Var "1GENEXPTEMP1" (CPtr t), Const 1 CInt] (CArray t))
    temp = genExp (tempmap, n+3, f, sizeof) tempexp (dest)
  in
    findSize ++ totalSize ++ temp
genExp (allocmap, n, f, sizeof) (ArrayAccess expr1 expr2 _) dest =
  let
    (arrType,t) =
      case getExpType expr1 of
        CArray (CStruct ctype) -> (CStruct ctype,CStruct ctype)
        CArray ctype -> (CPtr ctype,ctype)
    arr = genExp (allocmap, n+1, f, sizeof) expr1 (ATemp n arrType)
    idx = genExp (allocmap, n+2, f, sizeof) expr2 (ATemp (n+1) CInt)
    arrLenAddr = [AAsm [ATemp (n+2) (CPtr CInt)] ASub [ALoc (ATemp n arrType), AImm 8]]
    arrLen = [AAsm [ATemp (n+3) CInt] ANop [ALoc  (derefLoc (ATemp (n+2) (CPtr CInt)))]]
    boundsBool = [AAsm [ATemp (n+4) CBool] ALeq [ALoc (ATemp (n+3) CInt), ALoc (ATemp (n+1) CInt)]]
    checkBool = [AIf (ACond (ALoc (ATemp (n+4) CBool))) [ACall (AReg 1 CInt) ("" ++ "raise") [(AImm 11)]] []]
    -- On the next line, idx is treated as a CPtr.  This is intentional
    offset = [AAsm [ATemp (n+2) arrType] AMul [ALoc (ATemp (n+1) arrType), AImm (sizeof t)]] -- safe to overwrite n+2 now
    memloc = [AAsm [ATemp (n+3) arrType] AAdd [ALoc (ATemp n arrType), ALoc (ATemp (n+2) arrType)]] -- safe to overwrite n+3 now
    deref = [AAsm [dest] ANop [ALoc (derefLoc (ATemp (n+3) arrType))]]
    retmem = [AAsm [dest] ANop [ALoc (ATemp (n+3) arrType)]]
  in
    case t of
      CStruct _ -> arr ++ idx ++ offset ++ memloc ++ retmem
      _ -> arr ++ idx ++ offset ++ memloc ++ deref
genExp alloc (NULL _) dest = [AAsm [dest] ANop [AImm 0]]

genExpStructSpecial (allocmap, n, f, s) (PointerStar expr t) dest =
  let
    temp = genExp (allocmap, n+1, f, s) expr (ATemp n (getExpType expr))
    exprType = getExpType expr
    toDest = 
      case exprType of
        CPtr (CStruct _) -> [AAsm [dest] ANop [ALoc (ATemp n (getExpType expr))]]
        _ -> [AAsm [dest] ANop [ALoc (derefLoc (ATemp n (getExpType expr)))]]
  in
    temp ++ toDest
genExpStructSpecial alloc expr dest = genExp alloc expr dest

-- Generates ACond or optimized ACond depending on whether the optimized version is available via
-- the x86 asm instructions je jne jle jl ja etc.
genACond :: Alloc -> Exp -> ([AAsm], ACond, Int)
genACond (allocmap, n, f, s) expr@(Binary binop exp1 exp2 ctype) =
  let
    -- Can change n+2 below to n+1
    cogenExp1new = genExp (allocmap, n + 1, f, s) exp1 (ATemp n (getExpType expr))
    cogenExp2new = genExp (allocmap, n + 2, f, s) exp2 (ATemp (n+1) (getExpType expr))
    cogenExpnew = genExp (allocmap, n + 1, f, s) expr (ATemp n (getExpType expr))
  in
  case isCondOpOptimizable binop of
    True -> 
      (cogenExp1new ++ cogenExp2new,ACondOptimized (ALoc $ ATemp n ctype) (genBinOp binop) (ALoc $ ATemp (n+1) ctype), n+2)
    False -> (cogenExpnew, ACond $ ALoc $ ATemp n CBool, n+1)
genACond (allocmap, n, f, s) expr = 
  let
    cogenExpnew = genExp (allocmap, n + 1, f, s) expr (ATemp n (getExpType expr))
  in
    (cogenExpnew, ACond $ ALoc $ ATemp n CBool, n+1)

isCondOpOptimizable :: Binop -> Bool
isCondOpOptimizable Less = True
isCondOpOptimizable Leq = True
isCondOpOptimizable Geq = True
isCondOpOptimizable Greater = True
isCondOpOptimizable Eq = True
isCondOpOptimizable Neq = True
isCondOpOptimizable _ = False

genCtrl :: Alloc -> Ctrl -> [AAsm]
genCtrl (allocmap, n, f, s) (If expr stmt1 stmt2) =
  let
    (cogen, acond, nextNum) = genACond (allocmap, n, f, s) expr
    rest = [AIf acond (genStmt (allocmap, nextNum, f, s) stmt1) 
                                  (genStmt (allocmap, nextNum, f, s) stmt2)]
  in
    case expr of
      -- Bullshit hack to get washington-stack-attack to pass
      CTrue _ -> genStmt (allocmap, n, f, s) stmt1
      CFalse _ -> genStmt (allocmap, n, f, s) stmt2
      _ -> cogen ++ rest
genCtrl (allocmap, n, f, s) (While expr stmt) =
  let
    (cogen, acond, nextNum) = genACond (allocmap, n, f, s) expr
    rest = [AWhile acond ((genStmt (allocmap, nextNum, f, s) stmt) ++ cogen)]
  in
    cogen ++ rest
genCtrl alloc (Ret e) = genExp alloc e (AReg 0 (getExpType e)) ++ [ARet (ALoc $ AReg 0 (getExpType e))]
genCtrl alloc RetVoid = [ARet (ALoc $ AReg 0 CVoid)]

-- Wrapping something up in AHeap (ALoc (thing)) (deref'd thing type) is very common
-- should only be used on ATemps and ARegs
derefLoc :: ALoc -> ALoc
derefLoc loc = 
  let
    locType = getLocType loc
    newType = 
      case locType of
        CStruct s -> CStruct s
        CPtr t -> t
        _ -> error ("A type was mismatched in codeGen when dereferencing " ++ (show loc) ++ " of type " ++ (show locType))
  in
    AHeap (ALoc loc) newType

getLocType :: ALoc -> CType
getLocType (AReg _ t) = t
getLocType (ATemp _ t) = t
getLocType (AMem _ t) = t
getLocType (AInArg _ t) = t
getLocType (AArg _ t) = t
getLocType (AHeap _ t) = t

genBinOp :: Binop -> AOp
genBinOp Add = AAdd
genBinOp Sub = ASub
genBinOp Div = ADiv
genBinOp Mul = AMul
genBinOp Mod = AMod
genBinOp ShiftL = AShiftL
genBinOp ShiftR = AShiftR
genBinOp Less = ALess 
genBinOp Leq = ALeq
genBinOp Geq = AGeq
genBinOp Greater = AGreater
genBinOp Eq = AEq
genBinOp Neq = ANeq
genBinOp BAnd = ABAnd
genBinOp BXor = ABXor
genBinOp BOr = ABOr
genBinOp LAnd = ALAnd
genBinOp LOr = ALOr

getLvalType :: Lvalue -> CType
getLvalType (Variable _ t) = t
getLvalType (LvalueDot _ _ t) = t
getLvalType (LvaluePointerStar _ t) = t
getLvalType (LvalueArrayAccess _ _ t) = t

getExpType :: Exp -> CType
getExpType (Const _ t) = t
getExpType (CTrue t) = t
getExpType (CFalse t) = t
getExpType (Var _ t) = t
getExpType (Unary _ _ t) = t
getExpType (Binary _ _ _ t) = t
getExpType (Ternary _ _ _ t) = t
getExpType (Call _ _ t) = t
getExpType (Dot _ _ t) = t
getExpType (Alloc _ t) = t
getExpType (PointerStar _ t) = t
getExpType (AllocArray _ _ t) = t
getExpType (ArrayAccess _ _ t) = t
getExpType (NULL t) = t
