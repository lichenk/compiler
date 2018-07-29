module Compile.Types.LLVM where
import Data.List
import Compile.Constants

data LLoc = LTemp Int LType
	   			| LImm Int LType
	   			| LHeap LLoc LType deriving(Eq,Ord) -- ONLY used in intermediate stage!

-- Type of function, Function name, Argument List, Instruction list
data LFdecl = LFdecl LType String [LLoc] [LInstr]

-- Type of function, Function name, Argument List, Instruction list
-- declare noalias i8* @malloc(i64) nounwind
data LHeaderFdecl = LHeaderFdecl LType String [LType]

-- StructName [Field type, Field name]
data LSdecl = LSdecl String [(LType, String)]

data LType = LPtr LType -- Pointer to type
      | LInt8 --void*
			| LInt32 -- Called I32 in LLVM Langref
			| LInt64 -- Called I64
			| LBool -- Called I1 in LLVM Langref
			| LArray LType
			| LStruct String
			| LVoid
      | LDebug -- Do not use!
			| LReserved deriving(Eq, Ord) -- ONLY used in intermediate stage!

				-- Binop Destination BinopType Source1 Source2
data LInstr = LInstr LLoc LBinop LLoc LLoc
			-- Icmp Destination Comparison_operator Source1 Source2
			| LIcmp LLoc LCond LLoc LLoc
			-- <result> = alloca dest type
			| LAlloca LLoc LType
			-- Call Destination FunctionType FunctionName ArgumentList
			| LCall LLoc String [LLoc]
			-- Destination, Type of Source Pointer, Source Pointer
			| LLoad LLoc LLoc
			-- pointer_type, pointer_to_write_to, value_type, value_to_write
			| LStore LLoc LLoc
			-- Boolean_variable, label <if_variable_is_true>, label <if_variable_is_false>
			| LBrCond LLoc Int Int
      | LGetelementptrInstr LLoc LLoc [LLoc]
			-- Unconditional jump to label
			| LBr Int
      | LLabel Int
      -- dest, src
			| LMove LLoc LLoc
			| LIf LLoc [LInstr] [LInstr]  -- ONLY used in intermediate stage!
			| LWhile LLoc [LInstr]  -- ONLY used in intermediate stage!
			| LRet LLoc
			| LRetVoid
      | LBitcast LLoc LLoc LType
      | LStr String

data LCond = LEq
      | LNe
      | LUgt
      | LUge
      | LUlt
      | LUle
      | LSgt
      | LSge
      | LSlt
      | LSle

-- Possible binops are:
-- add sub smul sdiv srem ashl ashr
-- or xor and
-- Possibly need umul for array
data LBinop = LSAdd
			 | LSSub
			 | LSMul
			 | LSDiv
			 | LSRem
			 | LAShl
			 | LAShr
			 | LBOr
			 | LBXor
			 | LBAnd
			 | LUMul
			 | LUAdd
			 | LGetelementptr

getLLocType :: LLoc -> LType
getLLocType (LTemp _ t) = t
getLLocType (LImm _ t) = t
getLLocType (LHeap _ t) = t

showArgs :: [LLoc] -> String
showArgs args = "(" ++ (intercalate ", " $ map (show) args) ++ ")"

showFieldTypes :: [(LType, String)] -> String
showFieldTypes fields = intercalate ", " $ map ((show) . fst) fields 

showInstrList :: [LInstr] -> String
showInstrList instructions =
  "{\n" ++ (unlines (map (\s -> "  " ++ s) (map (show) instructions))) ++ "\n}"

instance Show LSdecl where
  show (LSdecl name fields) = "%struct." ++ name ++ " = type <{ " ++ (showFieldTypes fields) ++ " }>"

instance Show LFdecl where
  show (LFdecl t name args body) = "define " ++ (show t) ++ " @" ++ name ++ (showArgs args) ++ " " ++ (showInstrList body)

-- declare noalias i8* @malloc(i64) nounwind
instance Show LHeaderFdecl where
  show (LHeaderFdecl t name args) = "declare " ++ (show t) ++ " @" ++ name ++ "(" ++ (intercalate ", " $ map show args) ++ ")"

instance Show LInstr where
  show (LInstr dest op src1 src2) = 
    case op of
      LSDiv -> (llocNoType dest) ++ " = call i32 @" ++ divName ++ (showArgs [src1,src2])
      LSRem -> (llocNoType dest) ++ " = call i32 @" ++ modName ++ (showArgs [src1,src2])
      -- LAShl -> (llocNoType dest) ++ " = call i32 @" ++ salName ++ (showArgs [src1,src2])
      -- LAShr -> (llocNoType dest) ++ " = call i32 @" ++ sarName ++ (showArgs [src1,src2])
      LGetelementptr -> (llocNoType dest) ++ " = " ++ (show op) ++ " " ++ (show src1) ++ ", " ++ (show (LImm 0 LInt32)) ++ ", " ++ (show src2)
      _ -> (llocNoType dest) ++ " = " ++ (show op) ++ " " ++ (show src1) ++ ", " ++ (llocNoType src2)
  show (LIcmp dest cmp src1 src2) =
    (llocNoType dest) ++ " = icmp " ++ (show cmp) ++ " " ++ (show src1) ++ ", " ++ (llocNoType src2)
  show (LAlloca dest t) =
    (llocNoType dest) ++ " = alloca " ++ (show t)
  show (LCall dest func args) =
    case getLLocType dest of
      LVoid -> "call void @" ++ func ++ (showArgs args)
      _ -> (llocNoType dest) ++ " = call " ++ (show (getLLocType dest)) ++ " @" ++ func ++ (showArgs args)
  show (LLoad dest src) =
    (llocNoType dest) ++ " = load " ++ (show (LPtr (getLLocType dest))) ++ " " ++ (llocNoType src)
  show (LStore destptr val) = 
    "store " ++ (show val) ++ ", " ++ (show (LPtr (getLLocType val))) ++ " " ++ (llocNoType destptr)
  show (LBrCond cond truelabel falselabel) =
    "br " ++ (show cond) ++ ", label %L" ++ (show truelabel) ++ ", label %L" ++ (show falselabel)
  show (LBr label) = "br label %L" ++ (show label)
  show (LLabel label) = "L" ++ (show label) ++ ":"
  show (LMove dest src) = 
    (llocNoType dest) ++ " = bitcast " ++ (show src) ++ " to " ++ (show (getLLocType src))
  show (LRet ret) = "ret " ++ (show ret)
  show LRetVoid = "ret void"
  show (LIf cond ifblk elseblk) = "if (" ++ (show cond) ++ ") " ++ (showInstrList ifblk) ++ "\nelse " ++ (showInstrList elseblk)
  show (LWhile cond body) = "while (" ++ (show cond) ++ ") " ++ (showInstrList body) 
  -- %3 = bitcast i8* %2 to %struct.foo*
  show (LBitcast dest src outType) =
    (llocNoType dest) ++ " = bitcast " ++ (show src) ++ " to " ++ (show outType)
  show (LStr s) = s
  show (LGetelementptrInstr dest struct fields) =
    let 
      fieldsStr = intercalate " ," $ map (show) (struct:fields)
    in
      (llocNoType dest) ++ " = getelementptr " ++ fieldsStr

-- Because destinations aren't given types, and the second argument of a binop isn't given a type
llocNoType :: LLoc -> String
llocNoType (LTemp n t) = 
  case n >= 0 of
    True -> "%s" ++ (show n) ++ (serializeType t)
    False -> "%r" ++ (show (-n)) ++ (serializeType t)
llocNoType (LImm n t) = 
  case (t,n == 0) of
    (LPtr _, True) -> "null"
    _ -> (show n)
llocNoType (LHeap n _) = "%H" ++ (show n)

instance Show LLoc where
  show (LTemp n t) =
    case (n >= 0) of
      True -> (show t) ++ " %s" ++ (show n) ++ (serializeType t)
      False -> (show t) ++ " %r" ++ (show (-n)) ++ (serializeType t)
  show (LImm n t) =
    case (t,n == 0) of
      (LPtr _, True) -> (show t) ++ " " ++ "null"
      _ -> (show t) ++ " " ++ (show n)
  show (LHeap n t) = (show t) ++ " %H" ++ (show n)

instance Show LType where
   show LInt8 = "i8"
   show LInt32 = "i32"
   show LInt64 = "i64"
   show LBool = "i1"
   show (LArray t) = "[0 x " ++ (show t) ++ " ]"
   show (LStruct str) = "%struct." ++ str
   show LVoid = "void"
   show (LPtr LVoid) = (show LInt8) ++ "*"
   show (LPtr t) = (show t) ++ "*"
   show LReserved = "RESERVED" 
   show LDebug = "DEBUGTYPE"

instance Show LCond where
  show LEq = "eq"
  show LNe = "ne"
  show LUgt = "ugt"
  show LUlt = "ult"
  show LUge = "uge"
  show LUle = "ule"
  show LSlt = "slt"
  show LSgt = "sgt"
  show LSge = "sge"
  show LSle = "sle"

instance Show LBinop where
  show LSAdd = "add"
  show LSSub = "sub"
  show LSMul = "mul"
  show LSDiv = "sdiv"
  show LSRem = "srem"
  show LAShl = "shl"
  show LAShr = "ashr"
  show LBOr = "or"
  show LBXor = "xor"
  show LBAnd = "and"
  show LUMul = "mul"
  show LUAdd = "add"
  show LGetelementptr = "getelementptr"

serializeType :: LType -> String
serializeType LInt8 = "i8"
serializeType LInt32 = "i32"
serializeType LInt64 = "i64"
serializeType LBool = "i1"
serializeType (LStruct str) = "struct" ++ str ++ "z"
serializeType LVoid = "void"
--serializeType (LPtr LVoid) = "ptr" ++ (serializeType LInt8) ++ "z"
serializeType (LPtr t) = "ptr" ++ (serializeType t) ++ "z"
serializeType (LArray t) = "larr" ++ (serializeType t) ++ "z"
serializeType (LReserved) = "RESERVED"
serializeType (LDebug) = "DEBUGTYPE"

typifyLoc t (LTemp n _) = (LTemp n t)
typifyLoc t (LImm n _) = (LImm n t)
typifyLoc t (LHeap lloc _) = (LHeap lloc t)
