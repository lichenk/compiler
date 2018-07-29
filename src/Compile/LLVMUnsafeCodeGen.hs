module Compile.LLVMUnsafeCodeGen(llvmcodeGen,fuckType,astParamsToLLVMParams,ctypeToLtype) where
import Compile.Constants
import Compile.Types
import Compile.Types.LLVM
import Compile.LLVMStructGen
import Debug.Trace
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Compile.MapWrap as MapWrap
-- Constants
arrayLenMemberOffset = LImm 0 LInt32 --Field number of array length
arrayLarrayMemberOffset = LImm 1 LInt32 -- Field number of larray
lnothing = (LTemp (-99999999999) LInt32)

-- The first maps variables to their temps, the second is
-- the current number of temps, the third is field offset lookup for structs,
-- the fourth is a sizeof operator
type Alloc = (Map.Map String Int, Int, (LType -> String -> (Int, LType)), CType -> Int, Map.Map String LType)

-- define i32 @f(i32 %x, i32 %y) nounwind uwtable
functionDef :: AST -> LInstr
functionDef (Fdefn t name params stmts) =
  let
    fields = List.intercalate ", " (map (show) (astParamsToLLVMParams params))
  in
    LStr ("define " ++ (show t) ++ " @" ++ name ++ "(" ++ fields ++ ") nounwind uwtable {")

showParam :: Param -> String
showParam (Param t ident) = (show t) ++ " %" ++ ident


-- Really only going to work for functions.
-- Make sure you filter out the other stuff!
llvmcodeGen :: (CType -> Int, LType -> String -> (Int, LType)) -> Map.Map String LType -> AST -> [LInstr]
llvmcodeGen (sizeof, fields) functionTypeMap ast@(Fdefn t n params stmts) = 
  let
    arglist = map (\(Param t i) -> (t, i)) params
    numargs = length params
    decls = (map snd arglist) ++ (getDecls stmts)
    temps = Map.fromList $ zip decls [numargs..]
    alloc = (temps, (length decls + numargs), fields, sizeof, functionTypeMap)
    def = functionDef ast
    moveParams = map (makeMove numargs) (zip params [0..])
  in
    moveParams ++ (genBlock alloc stmts)

makeMove numargs ((Param t _), n) =
  (LMove (LTemp (n + numargs) (fuckType $ convertType t)) (LTemp n (fuckType $ convertType t)))

genBlock :: Alloc -> Block -> [LInstr]
genBlock alloc blk = concatMap (genStmt alloc) blk

genStmt :: Alloc -> Stmt -> [LInstr]
genStmt alloc (Simp simp) = genSimp alloc simp
genStmt alloc (Ctrl ctrl) = genCtrl alloc ctrl
genStmt alloc (Blk block) = genBlock alloc block

genSimp :: Alloc -> Simp -> [LInstr]
genSimp alloc@(allocmap, n, f, s, z) (Asgn lval op expr) = genAsgn alloc op lval expr
genSimp alloc (Decl _ _) = []
genSimp (allocmap, n, f, s, z) (Exp expr) = fst $ genExp (allocmap, n + 1, f, s, z) expr (LTemp  n LReserved)

-- In the case of variables, returns the temp to be written to, otherwise returns the memory address to write to.
-- May use the current temp to write to, so increment the current temp number (n)
genLval :: Alloc -> Lvalue -> ([LInstr], LLoc)
genLval (allocmap, _, _, _, _) (Variable ident t) = 
  let --FUCK I thought I edited this already!! Am I trapped in a time loop????
    realVarType = case t of
      CStruct i -> CPtr (CStruct i)
      _ -> t
    destType = ctypeToLtype realVarType
  in
  ([], LTemp (allocmap Map.! ident) destType)
genLval (allocmap, n, f, s, z) l@(LvalueDot lval field t) = 
  let
    (buildStruct, parentStruct, structType) = evalLvalAsExp (allocmap, n+1, f, s, z) lval
    (fieldNum, fieldType) = (f structType field)
    newTemp = LTemp n (LPtr fieldType)
    offset = [LInstr newTemp LGetelementptr parentStruct (LImm fieldNum LInt32)]
  in
    (buildStruct ++ offset, derefLoc newTemp)
genLval (allocmap, n, f, s, z) (LvaluePointerStar lval t) =
  let
    (buildPtr, ptr, _) = evalLvalAsExp (allocmap, n, f, s, z) lval
  in
    (buildPtr, derefLoc ptr)
genLval (allocmap, n, f, s, z) (LvalueArrayAccess lval expr ctype) =
  let
    idxTemp = (LTemp (n+2) LInt32)
    arrLenPtr = (LTemp (n+3) (LPtr LInt32))
    arrLenTemp = (LTemp (n+4) LInt32)
    boundsBoolTemp = (LTemp (n+5) LBool)
    larrayptrptr = (LTemp (n+6) (LPtr $ LPtr $ LArray t))
    larrayPtr = (LTemp (n+7) (LPtr $ LArray t))
    answer = (LTemp n (LPtr t))
    (buildArr, arr, arrType) = evalLvalAsExp (allocmap, n+1, f, s, z) lval
    t = ctypeToLtype ctype
    (idx,_) = genExp (allocmap, n+3, f, s, z) expr idxTemp
    -- arrLenAddr = [LInstr arrLenPtr LGetelementptr arr arrayLenMemberOffset]
    -- arrLen = [LLoad arrLenTemp arrLenPtr]
    -- boundsBool = [LIcmp boundsBoolTemp LSge idxTemp arrLenTemp]
    -- checkBool = [LIf boundsBoolTemp [LCall lnothing "raise" [LImm 11 LInt32]] []]
    -- memloc = [LGetelementptrInstr answer arr [LImm 0 LInt32, LImm 1 LInt32, LImm 0 LInt32, idxTemp]]
    larray = [LInstr larrayptrptr LGetelementptr arr arrayLarrayMemberOffset]
    larrayderef = [LLoad larrayPtr larrayptrptr]
    memloc = [LInstr answer LGetelementptr larrayPtr idxTemp]
  in
    (buildArr ++ idx ++ larray ++ larrayderef ++ memloc, derefLoc answer)

-- Does exactly what it says it does.  Evaluates an lval exactly as if it were an expression instead.  Always 
-- returns a temp
evalLvalAsExp :: Alloc -> Lvalue -> ([LInstr], LLoc, LType)
evalLvalAsExp (allocmap, n, f, s, z) (Variable ident t) = 
  let
    realVarType = case t of
      CStruct i -> CPtr (CStruct i)
      _ -> t
    destType = ctypeToLtype realVarType
  in
  ([], LTemp (allocmap Map.! ident) destType, destType)
evalLvalAsExp (allocmap, n, f, s, z) inast@(LvalueDot lval field t) =
  let
    structType = getLvalType lval
    (buildStruct, parentStruct, struct) = evalLvalAsExp (allocmap, n+1, f, s, z) lval
    fieldType :: LType
    (fieldNum, fieldType) = f struct field
    offset = [LInstr (LTemp (n+2) (LPtr fieldType)) LGetelementptr parentStruct (LImm fieldNum LInt32)]
    idest = LTemp n LReserved
    (moveToDest, dest, resultType) =
      case t of
        -- Yo! This is a hack!
        (CPtr (CStruct _)) ->
          let
            destType = LPtr fieldType
            dest = typifyLoc destType idest
          in
          ([LMove dest (LTemp (n+2) destType)], dest, destType)
        (CStruct _) ->
          let
            destType = LPtr fieldType
            dest = typifyLoc destType idest
          in
          ([LMove dest (LTemp (n+2) destType)], dest, destType)
        _ ->
          let
            destType = fieldType
            dest = typifyLoc destType idest
          in
            ([LLoad dest (LTemp (n+2) (LPtr destType))], dest, destType)
    ans = (buildStruct ++ offset ++ moveToDest, dest, resultType)
  in
    ans
evalLvalAsExp (allocmap, n, f, s, z) (LvaluePointerStar lval t) =
  let
    ptrType = getLvalType lval
    (buildPtr, ptrLoc, lvalType) = evalLvalAsExp (allocmap, n+1, f, s, z) lval
    destType =
      case ptrType of
        (CStruct _) -> lvalType
        _ -> derefLtype lvalType
    dest = typifyLoc destType (LTemp n LReserved)
    moveToDest =
      case ptrType of
        (CStruct _) -> [LMove dest ptrLoc]
        _ -> [LLoad dest ptrLoc]
  in
    (buildPtr ++ moveToDest, dest, destType)

-- evalLvalAsExp (allocmap, n, f, s, z) (LvalueArrayAccess lval expr _) = 
--   let
--     (arrType,destType) =
--       case getLvalType lval of
--         CArray (CStruct ctype) -> (CStruct ctype, CStruct ctype)
--         CArray ctype -> (CPtr ctype, ctype)
--     t = convertType destType
--     (buildArr, arr, _) = evalLvalAsExp (allocmap, n+1, f, s, z) lval
--     (idx,_) = genExp (allocmap, (n+3), f, s, z) expr (LTemp (n+2) LInt32)
--     arrLenAddr = [LInstr (LTemp (n+3) (LPtr LInt32)) LGetelementptr arr arrayLenMemberOffset]
--     arrLen = [LLoad (LTemp (n+4) LInt32) (LTemp (n+3) (LPtr LInt32))]
--     boundsBool = [LIcmp (LTemp (n+5) LBool) LSle (LTemp (n+2) LInt32) (LTemp (n+4) LInt32)]
--     checkBool = [LIf (LTemp (n+5) LBool) [LCall lnothing "raise" [LImm 11 LInt32]] []]
--     larray = [LInstr (LTemp (n+6) (LPtr $ LArray t)) LGetelementptr arr arrayLarrayMemberOffset]
--     memloc = [LInstr (LTemp (n+7) (LPtr t)) LGetelementptr (LTemp (n+6) (LPtr $ LArray t)) (LTemp (n+2) LInt32)]
--     deref = [LLoad (LTemp n t) (LTemp (n+7) (convertType arrType))]
--     retmem = [LMove (LTemp n t) (LTemp (n+7) (convertType arrType))]
--   in
--     case destType of
--       CStruct _ -> (buildArr ++ idx ++ arrLenAddr ++ arrLen ++ boundsBool ++ checkBool ++ larray ++ memloc ++ retmem, LTemp n t, t)
--       _ -> (buildArr ++ idx ++ arrLenAddr ++ arrLen ++ boundsBool ++ checkBool ++ larray ++ memloc ++ deref, LTemp n t, t)

evalLvalAsExp (allocmap, n, f, s, z) (LvalueArrayAccess expr1 expr2 _) = 
  let
    -- arrStructPtrLabel = n
    lastLabel = n+10
    (buildArr, arrStructPtr, arrStructType) = evalLvalAsExp (allocmap, lastLabel, f, s, z) expr1
    -- Perform a field lookup in the array struct to get the type
    (_, LPtr (LArray t)) = f arrStructType "arr" -- t is the type of the array
    -- (convertType (CArray ctype))
    -- arrStructPtr = LTemp arrStructPtrLabel (LPtr $ LStruct (makeArrayStructNameL t))
    idxTemp = (LTemp (n+2) LInt32)
    arrLenPtr = (LTemp (n+3) (LPtr LInt32))
    arrLenTemp = (LTemp (n+4) LInt32)
    boundsBoolTemp = (LTemp (n+5) LBool)
    larrayptrptr = (LTemp (n+6) (LPtr $ LPtr $ LArray t))
    larrayPtr = (LTemp (n+7) (LPtr $ LArray t))
    elementPtr = (LTemp (n+8) (LPtr $ t))
    idest = LTemp n LReserved
    typedDest = typifyLoc t idest
    typedPtrDest = typifyLoc (LPtr t) idest
    -- End of temps declarations
    (idx, LInt32) = genExp (allocmap, lastLabel, f, s, z) expr2 idxTemp
    -- arrLenAddr = [LInstr arrLenPtr LGetelementptr arrStructPtr arrayLenMemberOffset]
    -- arrLen = [LLoad arrLenTemp arrLenPtr]
    -- boundsBool = [LIcmp boundsBoolTemp LSge idxTemp arrLenTemp]
    -- checkBool = [LIf boundsBoolTemp [LCall lnothing "raise" [LImm 11 LInt32]] []]
    larray = [LInstr larrayptrptr LGetelementptr arrStructPtr arrayLarrayMemberOffset]
    larrayderef = [LLoad larrayPtr larrayptrptr]
    memloc = [LInstr elementPtr LGetelementptr larrayPtr idxTemp]
    loadArrayElement = [LLoad typedDest elementPtr]
    moveArrayElementPtr = [LMove typedPtrDest elementPtr]
  in
    case t of
      -- If array is an array of LStructs, return a pointer to the array element
      LStruct _ -> (buildArr ++ idx ++ larray ++ larrayderef ++ memloc ++ moveArrayElementPtr, typedPtrDest, LPtr t)
      -- Else, return the array element
      _ -> (buildArr ++ idx ++ larray ++ larrayderef ++ memloc ++ loadArrayElement, typedDest, t)


genAsgn :: Alloc -> Asnop -> Lvalue -> Exp -> [LInstr]
genAsgn alloc@(allocmap, n, f, s, z) Equal lval expr =
  let 
    (buildDest, destination) = genLval alloc lval
    (buildExpr, expType) = genExp (allocmap, n+2, f, s, z) expr (LTemp (n+1) LReserved)
  in
    case destination of
      LHeap destPtr _ -> buildDest ++ buildExpr ++ [LStore destPtr (LTemp (n+1) expType)]
      _ -> buildDest ++ buildExpr ++ [LMove destination (LTemp (n+1) expType)]
genAsgn alloc@(allocmap, n, f, s, z) (AsnOp bin) lval expr =
  let
    (buildDest, destinationRaw) = genLval alloc lval
    destType = getLocType destinationRaw
  in
    case destinationRaw of
    (LHeap destination _) ->
      let
        (realDestType) = destType
        lvalType = getLvalType lval
        exprType = getExpType expr
        (buildExpr,_) = genExp (allocmap, n+2, f, s, z) expr (LTemp (n+1) LReserved)
        derefDest = [LLoad (LTemp (n+2) destType) destination]
        -- So now the expression's value is in n+1 and the LHS's existing value is in n+2
        -- Horrible hack so that we can reuse genExp for this.  Making temporary variables which hold what was computed above.
        tempmap = Map.union allocmap $ Map.fromList [("1GENASGNTEMP1", n+1), ("2GENASGNTEMP2", n+2)]
        tempExp = (Binary bin (Var "2GENASGNTEMP2" exprType) (Var "1GENASGNTEMP1" exprType) lvalType)
        tempDest = LTemp (n+3)  realDestType
        (assign,_) = genExp (tempmap, n+3, f, s, z) tempExp tempDest
        moveStore = [LStore destination tempDest]
      in
        buildDest ++ buildExpr ++ derefDest ++ assign ++ moveStore
    destination ->
      let
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
    ([LMove dest (LImm 0 LBool)], destType)
genExp (allocmap, _, _, _, _) (Var var t) idest = 
  let
    realVarType = case t of
      CStruct i -> CPtr (CStruct i)
      _ -> t
    destType = ctypeToLtype realVarType
    dest = typifyLoc destType idest
  in
    ([LMove dest (LTemp (allocmap Map.! var) destType)], destType)
genExp (allocmap, n, f, s, z) (Unary unop exp _) dest =
  let
    (cogen, tempnType) = genExp (allocmap, n + 1, f, s, z) exp (LTemp n LReserved)
    assign = 
      case unop of
        Neg -> [LInstr (typifyLoc LInt32 dest) LSSub (LImm 0 LInt32) (LTemp n tempnType)]
        Flip -> [LInstr (typifyLoc LInt32 dest) LBXor (LImm 4294967295 LInt32) (LTemp n tempnType)]
        Not -> [LInstr (typifyLoc LBool dest) LSSub (LImm 1 LBool) (LTemp n tempnType)]
  in
    (cogen ++ assign, tempnType)
genExp alloc@(allocmap, n, f, s, z) expre@(Binary binop exp1 exp2 _) dest = 
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
        isCmp = isBinopCmp binop
        (combine, finalType) = case isCmp of
          False -> 
            ([LInstr (typifyLoc cogen1Type dest) (genBinOp binop) (LTemp n cogen1Type) (LTemp (n + 1) cogen2Type)],cogen1Type)
          True -> ([LIcmp (typifyLoc LBool dest) (genCmp binop) (LTemp n cogen1Type) (LTemp (n + 1) cogen2Type)], LBool)
      in (cogen1 ++ cogen2 ++ cogen3 ++ combine, finalType)
genExp (allocmap, n, f, s, z) (Ternary exp1 exp2 exp3 _) dest =
  let
    (cogen1, _) = genExp (allocmap, n + 1, f, s, z) exp1 (LTemp n LBool)
    (cogen2, cogen2Type) = genExp (allocmap, n + 2, f, s, z) exp2 dest
    (cogen3, _) = genExp (allocmap, n + 3, f, s, z) exp3 dest
  in
    (cogen1 ++ [LIf (LTemp n LBool) cogen2 cogen3], cogen2Type)
genExp (allocmap, n, f, s, z) (Call func args calltype) idest =
  let
    destType = case (Map.member func z, func == "raise", func == "calloc") of
      (_,_,True) -> LPtr LInt8
      (True, _,_) -> z Map.! func
      (False, True,_) -> convertType calltype
      (_,_,_) -> trace ("Warning: Using untrusted type info for call:" ++ show func ++ " with type " ++ show calltype) $ convertType calltype
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
            ([LLoad dest (LTemp (n+1) (LPtr fieldType))], destType)
  in
    (structgen ++ offset ++ moveToDest, destType2)
genExp alloc@(allocmap, n, f, sizeof, z) (Alloc t _) idest = 
  let
    numBytes = sizeof t
    destType = LPtr $ convertType t
    dest = typifyLoc destType idest
    (callocCode,_) = genExp (allocmap, n+1, f, sizeof, z) (Call ("calloc") [Const numBytes CInt, Const 1 CInt] (CPtr CInt8)) (LTemp n (LPtr LInt8))
  in
    (callocCode ++ [LBitcast dest (LTemp n (LPtr LInt8)) destType], destType)
genExp (allocmap, n, f, s, z) (PointerStar expr t) idest =
  let
    (temp, expType) = genExp (allocmap, n+1, f, s, z) expr (LTemp n LReserved)
    src = LTemp n expType
  in
    case expType of
      (LPtr (LStruct struct)) ->
        let
          t = LStruct struct
          dest = typifyLoc t idest
        in
          (temp ++ [LMove dest src], t)
      (LPtr t) ->
        let
          dest = typifyLoc t idest
        in
          (temp ++ [LLoad dest src], t)

genExp (allocmap, n, f, sizeof, z) (AllocArray ctype expr _) dest =
  let
    t = convertType ctype
    -- Temp declarations
    arrayPtr = (LTemp n (LPtr $ LInt8))
    arrayLen = (LTemp (n+1) LInt32)
    arrayBytes = (LTemp (n+2) LInt32)
    arrayStructPtr = LTemp (n+3) ((convertType (CArray ctype)))
    arraySizePtr = (LTemp (n+4) (LPtr LInt32))
    arrayArrayPtr = (LTemp (n+5) (LPtr $ LPtr $ LArray t))
    arrayPtrCasted = (LTemp (n+6) (LPtr $ LArray t))
    nextTemp = n+8
    -- Code starts here!
    -- First allocate actual array
    numBytes = (sizeof ctype)
    findSize :: [LInstr]
    (findSize,_) = genExp (allocmap, nextTemp, f, sizeof, z) expr arrayLen
    totalSize :: [LInstr]
    totalSize = [LInstr arrayBytes LUMul arrayLen (LImm numBytes LInt32)]
    -- Now t(n+2) contains the size I want to pass to calloc
    -- I'll use the same horrible hack as way above to call calloc.  This makes sure that you aren't
    -- passing negatives to calloc.
    tempmap = Map.union allocmap $ Map.fromList [("1GENEXPTEMP1", (n+2)), ("1SIZE1", (n+1))]
    tempexp = 
      Ternary 
        (Binary Geq (Var "1SIZE1" CInt) (Const 0 CInt) CBool) 
        (Call ("calloc") [Var "1GENEXPTEMP1" CInt, Const 1 CInt] (CPtr ctype)) 
        (Call ("raise") [(Const 11 CInt)] (CInt)) (CInt)
    temp :: [LInstr]
    (temp,_) = genExp (tempmap, nextTemp, f, sizeof, z) tempexp arrayPtr
    -- Now, I need to allocate an array structure of type
    -- struct {int32 length, t[] array}
    allocArrayStruct :: [LInstr]
    (allocArrayStruct,_) = genExp (allocmap, nextTemp, f, sizeof, z) (Alloc (CRawArray ctype) (CArray ctype)) arrayStructPtr
    movesize :: [LInstr]
    movesize = [LInstr arraySizePtr LGetelementptr arrayStructPtr arrayLenMemberOffset,
      LStore arraySizePtr arrayLen]
    moveArrayPtr :: [LInstr]
    moveArrayPtr = [LBitcast arrayPtrCasted arrayPtr (LPtr $ LArray t),
      LInstr arrayArrayPtr LGetelementptr arrayStructPtr arrayLarrayMemberOffset,
      LStore arrayArrayPtr arrayPtrCasted]
    finalType = convertType (CArray ctype)
    moveToDest :: [LInstr]
    moveToDest = [LMove (typifyLoc finalType dest) arrayStructPtr]
  in
    (findSize ++ totalSize++ temp ++ allocArrayStruct ++ movesize ++ moveArrayPtr ++ moveToDest, finalType)

genExp (allocmap, n, f, s, z) dbg@(ArrayAccess expr1 expr2 _) idest =
  let
    arrStructPtrLabel = n
    lastLabel = n+10
    (buildArr, arrStructType) = genExp (allocmap, lastLabel, f, s, z) expr1 (LTemp arrStructPtrLabel LReserved)
    -- Perform a field lookup in the array struct to get the type
    (_, LPtr (LArray t)) = f arrStructType "arr" -- t is the type of the array
    -- (convertType (CArray ctype))
    arrStructPtr = LTemp arrStructPtrLabel (LPtr $ LStruct (makeArrayStructNameL t))
    idxTemp = (LTemp (n+2) LInt32)
    arrLenPtr = (LTemp (n+3) (LPtr LInt32))
    arrLenTemp = (LTemp (n+4) LInt32)
    boundsBoolTemp = (LTemp (n+5) LBool)
    larrayptrptr = (LTemp (n+6) (LPtr $ LPtr $ LArray t))
    larrayPtr = (LTemp (n+7) (LPtr $ LArray t))
    elementPtr = (LTemp (n+8) (LPtr $ t))
    typedDest = typifyLoc t idest
    typedPtrDest = typifyLoc (LPtr t) idest
    -- End of temps declarations
    (idx, LInt32) = genExp (allocmap, lastLabel, f, s, z) expr2 idxTemp
    -- arrLenAddr = [LInstr arrLenPtr LGetelementptr arrStructPtr arrayLenMemberOffset]
    -- arrLen = [LLoad arrLenTemp arrLenPtr]
    -- boundsBool = [LIcmp boundsBoolTemp LSge idxTemp arrLenTemp]
    -- checkBool = [LIf boundsBoolTemp [LCall lnothing "raise" [LImm 11 LInt32]] []]
    larray = [LInstr larrayptrptr LGetelementptr arrStructPtr arrayLarrayMemberOffset]
    larrayderef = [LLoad larrayPtr larrayptrptr]
    memloc = [LInstr elementPtr LGetelementptr larrayPtr idxTemp]
    loadArrayElement = [LLoad typedDest elementPtr]
    moveArrayElementPtr = [LMove typedPtrDest elementPtr]
  in
    case t of
      -- If array is an array of LStructs, return a pointer to the array element
      LStruct _ -> (buildArr ++ idx ++ larray ++ larrayderef ++ memloc ++ moveArrayElementPtr, LPtr t)
      -- Else, return the array element
      _ -> (buildArr ++ idx ++ larray ++ larrayderef ++ memloc ++ loadArrayElement, t)


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
genBinOp b = error ("Unknown genbinop:" ++ show b)

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

derefLtype (LPtr t) = t
derefLtype t = error ("Cannot deref ltype:" ++ show t)

ctypeToLtype :: CType -> LType
ctypeToLtype t = convertType t

{-  case t of
    CInt -> LInt32
    CBool -> LBool
    CTypeIdent i -> error "We should have gotten rid of idents already"
    CVoid -> LVoid
    CPtr innerType -> LPtr $ convertType innerType
    CArray innerType -> LStruct (arrayStructPrefix ++ show (innerType)
    CStruct i -> LPtr (LStruct i)
    CAny -> error "Why is there a CAny here?"
    CEightByte -> LInt64
    CNoType -> error "Why is there a CNoType here?"
-}

isBinopCmp :: Binop -> Bool
isBinopCmp Less = True
isBinopCmp Leq = True
isBinopCmp Geq = True
isBinopCmp Greater = True
isBinopCmp Eq = True
isBinopCmp Neq = True
isBinopCmp _ = False

genCmp :: Binop -> LCond
genCmp Less = LSlt
genCmp Leq = LSle
genCmp Greater = LSgt
genCmp Geq = LSge
genCmp Eq = LEq
genCmp Neq = LNe

astParamsToLLVMParams :: [Param] -> [LLoc]
astParamsToLLVMParams params =
  let
    getType (Param t _) = t
    enumParams = List.zip [0 .. ] $ map getType params
  in
    map (\(num, t) -> LTemp num (fuckType $ ctypeToLtype t)) enumParams

fuckType (LStruct t) = LPtr(LStruct t)
fuckType t = t