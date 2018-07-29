module Compile.LLVMCodeGen where
import Compile.Types
import Compile.Types.LLVM
import Compile.LLVMStructGen
import qualified Data.Map as Map
import qualified Compile.MapWrap as MapWrap
-- Constants
arrayLenMemberOffset = LImm 0 LInt32 --Field number of array length
arrayLarrayMemberOffset = LImm 1 LInt32 -- Field number of larray
lnothing = (LTemp (-1234) LVoid)

-- The first maps variables to their temps, the second is
-- the current number of temps, the third is field offset lookup for structs,
-- the fourth is a sizeof operator
type Alloc = (Map.Map String Int, Int, (LType -> String -> (Int, LType)), CType -> Int, Map.Map String LType)


-- Really only going to work for functions.
-- Make sure you filter out the other stuff!
codeGen :: (CType -> Int, CType -> String -> Int) -> AST -> [LInstr]
codeGen (sizeof, fields, functionTypeMap) (Fdefn t n params stmts) = 
  let
    arglist = map (\(Param t i) -> (t, i)) params
    decls = (getDecls stmts) ++ (map snd arglist)
    temps = Map.fromList $ zip decls [0..]
    alloc = (temps, (length decls), fields, sizeof, functionTypeMap)
  in
    (genBlock alloc stmts)

genBlock :: Alloc -> Block -> [LInstr]
genBlock alloc blk = concatMap (genStmt alloc) blk

genStmt :: Alloc -> Stmt -> [LInstr]
genStmt alloc (Simp simp) = genSimp alloc simp
genStmt alloc (Ctrl ctrl) = genCtrl alloc ctrl
genStmt alloc (Blk block) = genBlock alloc block

genSimp :: Alloc -> Simp -> [LInstr]
genSimp alloc@(allocmap, n, f, s, z) (Asgn lval op expr) = genAsgn alloc op lval expr
genSimp alloc (Decl _ _) = []
genSimp (allocmap, n, f, s, z) (Exp expr) = genExp (allocmap, n + 1, f, s, z) expr (LTemp  n (getExpType expr))

-- In the case of variables, returns the temp to be written to, otherwise returns the memory address to write to.
-- May use the current temp to write to, so increment the current temp number (n)
genLval :: Alloc -> Lvalue -> ([LInstr], LLoc, LType)
genLval (allocmap, _, _, _, _) (Variable ident t) = ([], LTemp (allocmap Map.! ident) t)
genLval (allocmap, n, f, s, z) l@(LvalueDot lval field t) = 
  let
    structType = getLvalType lval
    (buildStruct, parentStruct) = evalLvalAsExp (allocmap, n+1, f, s, z) lval
    newTemp = LTemp n (LPtr t)
    offset = [LInstr newTemp LGetelementptr parentStruct (f structType field)]
  in
    (buildStruct ++ offset, derefLoc newTemp)
genLval (allocmap, n, f, s, z) (LvaluePointerStar lval t) =
  let
    (buildPtr, ptr) = evalLvalAsExp (allocmap, n, f, s, z) lval
  in
    (buildPtr, derefLoc ptr)
genLval (allocmap, n, f, s, z) (LvalueArrayAccess lval expr t) =
  let
    (buildArr, arr) = evalLvalAsExp (allocmap, n+1, f, s, z) lval
    idx = genExp (allocmap, n+3, f, s, z) expr (LTemp (n+2) LInt32)
    arrLenAddr = [LInstr (LTemp n+3 (LPtr LInt32)) LGetelementptr arr arrayLenMemberOffset]
    arrLen = [LLoad (LTemp (n+4) LInt32) (LTemp n+3 (LPtr LInt32))]
    boundsBool = [LIcmp (LTemp (n+5) LBool) LSle (LTemp (n+2) LInt32) (LTemp (n+4) LInt32)]
    checkBool = [LIf (LTemp (n+5) LBool) [LCall lnothing "raise" [LImm 11 LInt32]] []]
    larray = [LInstr (LTemp (n+6) (LPtr $ LArray t)) LGetelementptr arr arrayLarrayMemberOffset]
    memloc = [LInstr (LTemp n (LPtr t)) LGetelementptr (LTemp (n+6) (LPtr $ LArray t)) (LTemp (n+2) LInt32)]
  in
    (buildArr ++ idx ++ arrLenAddr ++ arrLen ++ boundsBool ++ checkBool ++ larray ++ memloc, derefLoc (LTemp n (LPtr t)))

-- Does exactly what it says it does.  Evaluates an lval exactly as if it were an expression instead.  Always 
-- returns a temp
evalLvalAsExp :: Alloc -> Lvalue -> ([LInstr], LLoc)
evalLvalAsExp (allocmap, n, f, s, z) (Variable ident t) = ([], LTemp (allocmap Map.! ident) t)
evalLvalAsExp (allocmap, n, f, s, z) inast@(LvalueDot lval field t) =
  let
    structType = getLvalType lval
    (buildStruct, parentStruct) = evalLvalAsExp (allocmap, n+1, f, s, z) lval
    offset = [LInstr (LTemp (n+2) (CPtr t)) LGetelementptr parentStruct (f structType field)]
    (moveToDest, resultType) =
      case t of
        -- Yo! This is a hack!
        (CPtr (CStruct _)) ->
          ([LMove (LTemp n (CPtr t)) (LTemp (n+2) (CPtr t))], CPtr t)
        (CStruct _) ->
          ([LMove (LTemp n (CPtr t)) (LTemp (n+2) (CPtr t))], CPtr t)
        _ ->
          ([LLoad (LTemp n t) (LTemp (n+2) (CPtr t))], t)
    ans = (buildStruct ++ offset ++ moveToDest, LTemp n resultType)
  in
    ans
evalLvalAsExp (allocmap, n, f, s, z) (LvaluePointerStar lval t) =
  let
    ptrType = getLvalType lval
    (buildPtr, ptrLoc) = evalLvalAsExp (allocmap, n+1, f, s, z) lval
    (moveToDest, resultType) =
      case ptrType of
        (CStruct _) -> (LMove (LTemp n (CPtr t)) ptrLoc, CPtr t)
        _ -> (LLoad (LTemp n t) ptrLoc, t)
  in
    (buildPtr ++ moveToDest, LTemp n resultType)
evalLvalAsExp (allocmap, n, f, s, z) (LvalueArrayAccess lval expr _) = 
  let
    (arrType,t) =
      case getLvalType lval of
        CArray (CStruct ctype) -> (CStruct ctype, CStruct ctype)
        CArray ctype -> (CPtr ctype, ctype)
    (buildArr, arr) = evalLvalAsExp (allocmap, n+1, f, s, z) lval
    (idx,_) = genExp (allocmap, n+3, f, s, z) expr (LTemp (n+2) LInt32)
    arrLenAddr = [LInstr (LTemp n+3 (LPtr LInt32)) LGetelementptr arr arrayLenMemberOffset]
    arrLen = [LLoad (LTemp (n+4) LInt32) (LTemp n+3 (LPtr LInt32))]
    boundsBool = [LIcmp (LTemp (n+5) LBool) LSle (LTemp (n+2) LInt32) (LTemp (n+4) LInt32)]
    checkBool = [LIf (LTemp (n+5) LBool) [LCall lnothing "raise" [LImm 11 LInt32]] []]
    larray = [LInstr (LTemp (n+6) (LPtr $ LArray t)) LGetelementptr arr arrayLarrayMemberOffset]
    memloc = [LInstr (LTemp (n+7) (LPtr t)) LGetelementptr (LTemp (n+6) (LPtr $ LArray t)) (LTemp (n+2) LInt32)]
    deref = [LLoad (LTemp n t) (LTemp (n+7) arrType)]
    retmem = [LMove (LTemp n t) (LTemp (n+7) arrType)]
  in
    case t of
      CStruct _ -> (buildArr ++ idx ++ arrLenAddr ++ arrLen ++ boundsBool ++ checkBool ++ larray ++ memloc ++ retmem, LTemp n t)
      _ -> (buildArr ++ idx ++ arrLenAddr ++ arrLen ++ boundsBool ++ checkBool ++ larray ++ memloc ++ deref, LTemp n t)

genAsgn :: Alloc -> Asnop -> Lvalue -> Exp -> [LInstr]
genAsgn alloc@(allocmap, n, f, s, z) Equal lval expr =
  let 
    (buildDest, destination) = genLval alloc lval
    (buildExpr, expType) = genExp (allocmap, n+2, f, s, z) expr (LTemp (n+1) LReserved)
  in
    buildDest ++ buildExpr ++ [LMove destination (LTemp (n+1) expType)]
genAsgn alloc@(allocmap, n, f, s, z) (AsnOp bin) lval expr =
  let
    (buildDest, destination, destType) = genLval alloc lval
    lvalType = getLvalType lval
    exprType = getExpType expr
    (buildExpr,_) = genExp (allocmap, n+2, f, s, z) expr (LTemp (n+1) LReserved)
    derefDest = [LMove (LTemp (n+2) destType) destination]
    -- So now the expression's value is in n+1 and the LHS's existing value is in n+2
    -- Horrible hack so that we can reuse genExp for this.  Making temporary variables which hold what was computed above.
    tempmap = Map.union allocmap $ Map.fromList [("1GENASGNTEMP1", n+1), ("2GENASGNTEMP2", n+2)]
    tempExp = (Binary bin (Var "2GENASGNTEMP2" lvalType) (Var "1GENASGNTEMP1" exprType) lvalType)
    (assign,_) = genExp (tempmap, n+3, f, s, z) tempExp destination
  in
    buildDest ++ buildExpr ++ derefDest ++ assign



genExp :: Alloc -> Exp -> LLoc -> ([LInstr], LType)
genExp _ (Const n _) idest =
  let
    destType = LInt32
    dest = typifyLoc destType idest
  in
    ([LMove dest (LImm (fromIntegral n) LInt32)], destType)
genExp _ (CTrue _) idest = 
  let
    destType = LBool
    dest = typifyLoc destType idest
  in
    ([LMove dest (LImm 1 LBool)], destType)
genExp _ (CFalse _) idest = 
  let
    destType = LBool
    dest = typifyLoc destType idest
  in
    ([LMove dest (LImm 1 LBool)], destType)
genExp (allocmap, _, _, _, _) (Var var t) idest = 
  let
    destType = ctypeToLtype t
    dest = typifyLoc destType idest
  in
    ([LMove dest (LTemp (allocmap Map.! var) destType)], destType)
genExp (allocmap, n, f, s, z) (Unary unop exp _) dest =
  let
    (cogen, tempnType) = genExp (allocmap, n + 1, f, s, z) exp (LTemp n LReserved)
    assign = 
      case unop of
        Neg -> [LInstr dest LSSub (LImm 0 LInt32) (LTemp n tempnType)]
        Flip -> [LInstr dest LBXor (LImm 4294967295 LInt32) (LTemp n tempnType)]
        Not -> [LInstr dest LSSub (LImm 1 LInt32) (LTemp n tempnType)]
  in
    (cogen ++ assign, tempnType)
genExp alloc@(allocmap, n, f, s, z) (Binary binop exp1 exp2 _) dest = 
  case binop of
    LAnd -> genExp alloc (Ternary exp1 exp2 (CFalse CBool)(getExpType exp1)) dest
    LOr -> genExp alloc (Ternary exp1 (CTrue CBool) exp2 (getExpType exp1)) dest
    otherwise -> 
      let
        (cogen1, cogen1Type) = genExp (allocmap, n + 1, f, s, z) exp1 (LTemp n LReserved)
        (cogen2, cogen2Type) = genExp (allocmap, n + 2, f, s, z) exp2 (LTemp (n+1) LReserved)
        tempmap = Map.union allocmap $ Map.fromList [("1EXP", n), ("2EXP", n+1)]
        cogen3 = 
          let
            failE = (Binary LOr (Binary Less (Var "2EXP" (getExpType exp2)) (Const 0 CInt) CBool) (Binary Greater (Var "2EXP" (getExpType exp2)) (Const 31 CInt) CBool) CInt)
            cogen = genCtrl (tempmap, n+2, f, s, z) (If failE (Simp (Exp (Binary Div (Const 1 CInt) (Const 0 CInt) CInt))) (Blk []))
          in
            case binop of
              ShiftL -> cogen
              ShiftR -> cogen
              _ -> []
        combine = [LInstr dest (genBinOp binop) (LTemp n LInt32) (LTemp (n + 1) LInt32)]
      in (cogen1 ++ cogen2 ++ cogen3 ++ combine, LInt32)
genExp (allocmap, n, f, s, z) (Ternary exp1 exp2 exp3 _) dest =
  let
    (cogen1, _) = genExp (allocmap, n + 1, f, s, z) exp1 (LTemp n LBool)
    (cogen2, cogen2Type) = genExp (allocmap, n + 2, f, s, z) exp2 dest
    (cogen3, _) = genExp (allocmap, n + 3, f, s, z) exp3 dest
  in
    (cogen1 ++ [LIf (LTemp n LBool) cogen2 cogen3], cogen2Type)
genExp (allocmap, n, f, s, z) (Call func args _) idest =
  let
    destType = z Map.! func
    dest = typifyLoc destType idest
    argnums = [0.. ((length args) - 1)]
    argdests = zip args argnums
    exprsTypesTupleList = map (\(expr, destNum) -> genExp (allocmap, n + destNum + 1, f, s, z) expr (LTemp (n + destNum) LReserved)) argdests
    (exprs, exprTypes) = unzip exprsTypesTupleList
    argdeststypes = zip3 args argnums exprTypes
    argtemplist = map (\(expr, destNum, ltype) -> (LTemp (n + destNum) ltype)) argdeststypes
    m = n + (length args)
  in
    ((concat exprs) ++ [(LCall dest func argtemplist)], destType)
genExp (allocmap, n, fields, s, z) (Dot expr field t) idest =
  let
    structgen :: [LInstr]
    (structgen, struct) = genExpStructSpecial (allocmap, n + 1, fields, s, z) expr (LTemp n struct)
    (fieldNum, fieldType) = fields struct field
    offset = [LInstr (LTemp (n+1) (LPtr fieldType)) LGetelementptr (LTemp n struct) (LImm fieldNum LInt32)]
    (moveToDest, destType2) =
      case t of
        (CStruct _) ->
          let
            destType = LPtr fieldType
            dest = typifyLoc destType idest
          in
            ([LMove dest (LTemp (n+1) destType)], destType)
        _ ->
          let
            destType = fieldType
            dest = typifyLoc destType idest
          in
            ([LLoad dest (LTemp (n+1) struct)], destType)
  in
    (structgen ++ offset ++ moveToDest, destType2)
genExp alloc@(allocmap, n, f, sizeof, z) (Alloc t _) dest = 
  let
    numBytes = sizeof t
  in
    genExp alloc (Call ("calloc") [Const numBytes CInt, Const 1 CInt] (CPtr t)) dest
genExp (allocmap, n, f, s, z) (PointerStar expr t) idest =
  let
    (temp, expType) = genExp (allocmap, n+1, f, s, z) expr (LTemp n LReserved)
    dest = typifyLoc expType idest
    toDest = 
      case t of
        --CStruct _ -> [LInstr [dest] ANop [LLoc (LTemp n (getExpType expr))]]
        _ -> [LMove dest (LTemp n expType)]
  in
    (temp ++ toDest, expType)
{- DISABLED
genExp (allocmap, n, f, sizeof, z) (AllocArray t expr _) dest =
  let
    numBytes = (sizeof t)
    findSize = genExp (allocmap, n+2, f, sizeof, z) expr (LTemp (n+1) CInt)
    totalSize = [LInstr (LTemp (n+2) (CPtr t)) LUMul (LTemp (n+1) CInt), LImm numBytes]
    totalSizePlusLen = [LInstr [LTemp (n+3) (CPtr t)] LUAdd (LTemp (n+2) (CPtr t)) (LImm 8 (CPtr t))]
    -- Now t(n+3) contains the size I want to pass to calloc
    -- I'll use the same horrible hack as way above to call calloc.  This makes sure that you aren't
    -- passing negatives to calloc.
    tempmap = Map.union allocmap $ Map.fromList [("1GENEXPTEMP1", (n+3)), ("1SIZE1", (n+1))]
    tempexp = 
      Ternary 
        (Binary Geq (Var "1SIZE1" CInt) (Const 0 CInt) CBool) 
        (Call ("calloc") [Var "1GENEXPTEMP1" (CPtr t), Const 1 CInt] (CArray t)) 
        (Call ("raise") [(Const 11 CInt)] (CArray t)) (CArray t)
    temp = genExp (tempmap, n+4, f, sizeof, z) tempexp (LTemp n (CPtr t))
    writeSize = [LStore (LTemp n (CPtr CInt)) (LTemp (n+1) CInt)]
    finalPtr = [LInstr dest LUAdd (LTemp n (CPtr t)) (LImm 8 (CPtr t))]
  in
    findSize ++ totalSize ++ totalSizePlusLen ++ temp ++ writeSize ++ finalPtr
-}
genExp (allocmap, n, f, s, z) (ArrayAccess expr1 expr2 _) idest =
  let
    (buildArr, exp1Type) = genExp (allocmap, n+1, f, s, z) expr1 (LTemp n LReserved)
    
    arr = (LTemp n exp1Type)
    (arrType,t, destType) =
      case getExpType expr1 of
        CArray (CStruct ctype) -> (CStruct ctype, CStruct ctype, destType)
        CArray ctype -> (CPtr ctype, ctype, derefLtype destType)
    (idx, exp2Type) = genExp (allocmap, (n+3), f, s, z) expr2 (LTemp (n+2) LInt32)
    arrLenAddr = [LInstr (LTemp (n+3) (LPtr LInt32)) LGetelementptr arr arrayLenMemberOffset]
    arrLen = [LLoad (LTemp (n+4) LInt32) (LTemp (n+3) (LPtr LInt32))]
    boundsBool = [LIcmp (LTemp (n+5) LBool) LSle (LTemp (n+2) LInt32) (LTemp (n+4) LInt32)]
    checkBool = [LIf (LTemp (n+5) LBool) [LCall lnothing "raise" [LImm 11 LInt32]] []]
    larray = [LInstr (LTemp (n+6) (LPtr $ LArray destType)) LGetelementptr arr arrayLarrayMemberOffset]
    memloc = [LInstr (LTemp (n+7) (LPtr destType)) LGetelementptr (LTemp (n+6) (LPtr $ LArray destType)) (LTemp (n+2) LInt32)]
    dest = typifyLoc destType idest
    deref = [LLoad dest (LTemp (n+7) (LPtr destType))]
    retmem = [LMove dest (LTemp (n+7) destType)]
  in
    case t of
      CStruct _ -> (buildArr ++ idx ++ arrLenAddr ++ arrLen ++ boundsBool ++ checkBool ++ larray ++ memloc ++ retmem, destType)
      _ -> (buildArr ++ idx ++ arrLenAddr ++ arrLen ++ boundsBool ++ checkBool ++ larray ++ memloc ++ deref, destType)
genExp alloc (NULL _) idest = 
  let
    destType = LPtr LVoid
    dest = typifyLoc destType idest
  in
    ([LMove dest (LImm 0 destType)], destType)

genExpStructSpecial (allocmap, n, f, s, z) (PointerStar expr t) idest =
  let
    (temp, expType) = genExp (allocmap, n+1, f, s, z) expr (LTemp n LReserved)
    destType =
      case t of
        CStruct _ -> expType
        _ -> derefLtype expType
    dest = typifyLoc destType idest
    toDest = 
      case t of
        CStruct _ -> [LMove dest (LTemp n expType)]
        _ -> [LLoad dest (LTemp n expType)]
  in
    (temp ++ toDest, destType)
genExpStructSpecial alloc expr dest = genExp alloc expr dest


genCtrl :: Alloc -> Ctrl -> [LInstr]
genCtrl (allocmap, n, f, s, z) (If expr stmt1 stmt2) =
  let
    -- Bullshit hack to get washington-stack-attack to pass
    (newstmt1, newstmt2) = case expr of
      (CTrue _) -> (stmt1, Blk []) -- expr True: stmt 2 will never be executed, make it empty
      (CFalse _) -> (Blk [], stmt2) -- expr is False: stmt 1 will never be executed, make it empty
      _ -> (stmt1, stmt2)
    (cogen, exprType) = genExp (allocmap, n + 1, f, s, z) expr (LTemp n LReserved)
    rest = [LIf (LTemp n exprType) (genStmt (allocmap, n + 1, f, s, z) newstmt1) 
                                  (genStmt (allocmap, n + 1, f, s, z) newstmt2)]
  in
    cogen ++ rest
genCtrl (allocmap, n, f, s, z) (While exp stmt) =
  let
    (cogen, expType) = genExp (allocmap, n + 1, f, s, z) exp (LTemp n LReserved)
    rest = [LWhile (LTemp n expType) ((genStmt (allocmap, n + 1, f, s, z) stmt) ++ cogen)]
  in
    cogen ++ rest
genCtrl (allocmap, n, f, s, z) (Ret e) = 
  let
    (cogen, expType) = genExp (allocmap, n+1, f, s, z) e (LTemp n LReserved)
  in
    cogen ++ [LRet (LTemp n expType)]
genCtrl alloc RetVoid = [LRetVoid]

genBinOp :: Binop -> LBinop
genBinOp Add = LSAdd
genBinOp Sub = LSSub
genBinOp Div = LSDiv
genBinOp Mul = LSMul
genBinOp Mod = LSRem
genBinOp ShiftL = LAShl
genBinOp ShiftR = LAShr
genBinOp BAnd = LBAnd
genBinOp BXor = LBXor
genBinOp BOr = LBOr
genBinOp LAnd = LBAnd
genBinOp LOr = LBOr

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

-- Wrapping something up in AHeap (ALoc (thing)) (deref'd thing type) is very common
-- should only be used on ATemps and ARegs
derefLoc :: LLoc -> LLoc
derefLoc loc = 
  let
    locType = getLocType loc
    newType = 
      case locType of
        LStruct s -> LStruct s
        LPtr t -> t
        _ -> error ("A type was mismatched in codeGen when dereferencing " ++ (show loc) ++ " of type " ++ (show locType))
  in
    LHeap loc newType

-- Gets type of Loc
getLocType :: LLoc -> LType
getLocType (LTemp _ t) = t
getLocType (LImm  _ t) = t
getLocType (LHeap _ t) = t

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

typifyLoc t (LTemp n _) = (LTemp n t)
typifyLoc t (LImm n _) = (LImm n t)
typifyLoc t (LHeap lloc _) = (LHeap lloc t)

derefLtype (LPtr t) = t
derefLtype t = error ("Cannot deref ltype:" ++ show t)

ctypeToLtype :: CType -> LType
ctypeToLtype ctype = convertType ctype