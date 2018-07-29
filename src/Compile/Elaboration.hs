module Compile.Elaboration where

import qualified Compile.Types.EAST as E
import qualified Compile.Types.FAST as A 
import qualified Compile.MapWrap as MapWrap
import Numeric as Num
import Compile.Types.Ops
import qualified Data.Map as Map
import Compile.Constants
import Debug.Trace
import Data.Int
import Data.Bits

data TypeNameType = TypeDefName A.Ident
              | StructDeclName A.Ident deriving (Ord, Eq, Show)

-- FunctionDeclMap[f] returns (ReturnType, [Argument Type]) where ReturnType is the return value of the function f, and [Argument Type] is the list of types
-- of the arguments of f
type ElabFunctionDeclMap = Map.Map A.Ident (A.CType, [A.CType])

-- ElabTypedefMap[ident] returns the C type of the ident as defined by previous typedefs
type ElabTypedefMap = Map.Map TypeNameType A.CType

-- Tuple of the shit we defined above, to be passed around everywhere
type ElabState = (ElabTypedefMap, ElabFunctionDeclMap)

-- elaborate :: A.AST -> ElabState -> E.AST
-- elaborate ([A.Fdefn ctype ident paramlist body]) m = E.Func (elabBlock body m)

{- I had to pull this out as recursion instead of a 
map in order to special case declaration + assignment,
which needs to become two statements -}
elabBlock :: A.Block -> ElabState -> E.Block
elabBlock [] _ = []
elabBlock (x:xs) (typedefmap, functiondefmap) =
  let
    m = (typedefmap, functiondefmap)
  in
  case x of
    A.Simp (A.Decl (A.DeclAsgn t v e)) -> 
      (case (Map.member (TypeDefName v) typedefmap) of
        True -> error("DeclAssgn uses same ident as typedef!")
        False -> 
          let
            rightFuncs = getFunctionsInExpr e
            rightHandSide = elabExpr m e
            leftType = elabType t m
          in
            if (length rightFuncs) > 0
            then [E.Simp (E.Decl leftType (declarationPrefix ++ v)), E.Simp (E.Asgn (E.Variable (declarationPrefix ++ v)) Equal rightHandSide), E.Simp (E.Decl leftType v), E.Simp (E.Asgn (E.Variable v) Equal (E.Var (declarationPrefix ++ v)))] ++ (elabBlock xs m)
            else [E.Simp (E.Decl (elabType t m) v), E.Simp (E.Asgn (E.Variable v) Equal (elabExpr m e))] ++ (elabBlock xs m)
      )
    _ -> (elabStmt x m):(elabBlock xs m)


elabType :: A.CType -> ElabState -> E.CType
elabType A.CInt _ = E.CInt
elabType A.CBool _ = E.CBool
elabType A.CNoType _ = E.CNoType
elabType A.CVoid _ = E.CVoid
elabType (A.CTypeIdent ident) (typedefmap, functiondefmap) = 
  case Map.member (TypeDefName ident) typedefmap of
    False -> error("Cannot resolve typedef for type ident:" ++ ident)
    True -> (case typedefmap Map.! (TypeDefName ident) of
               A.CTypeIdent _ -> error "You either have a recursive typedef or something is wrong in our code"
               t -> elabType t (typedefmap, functiondefmap))
elabType (A.CPtr ctype) elabState = E.CPtr (elabType ctype elabState)
elabType (A.CArray ctype) elabState = E.CArray (elabType ctype elabState)
-- Don't lookup struct idents!!!! There's no need to resolve struct names.
elabType (A.CStruct ident) (typedefmap, functiondefmap) = E.CStruct ident

elabStmt :: A.Stmt -> ElabState -> E.Stmt
elabStmt simp@(A.Simp (A.Decl (A.DeclAsgn t v e))) m = E.Blk $ elabBlock [simp] m
elabStmt (A.Simp s) m = E.Simp (elabSimp s m)
elabStmt (A.Ctrl c) m = elabCtrl c m
elabStmt (A.Blk b) m = E.Blk (elabBlock b m)

elabSimp :: A.Simp -> ElabState -> E.Simp
elabSimp (A.Asgn lvalue asnop expr) m =
  E.Asgn (elabLvalue lvalue m) asnop (elabExpr m expr)
-- Force use of Asnop due to the test case: foo[bar()] += 1;
--  case asnop of
--    Equal -> E.Asgn (elabLvalue lvalue m) (elabExpr m expr)
--    AsnOp op -> E.Asgn (elabLvalue lvalue m) (E.Binary op (elabLvalueToExpr lvalue m) (elabExpr m expr))
elabSimp (A.Post lvalue postop) m =
  let
    asnop = case postop of
      Inc -> AsnOp Add
      Dec -> AsnOp Sub
  in
    E.Asgn (elabLvalue lvalue m) asnop (E.Const 1)

    -- E.Asgn (elabLvalue lvalue m) (E.Binary op (elabLvalueToExpr lvalue m) (E.Const 1))
elabSimp (A.Decl (A.JustDecl t v)) (typedefmap, functiondefmap) =  -- We intentionally leave out decl assign
  case (Map.member (TypeDefName v) typedefmap) of
    True -> error("DeclAssgn uses same ident as typedef!")
    False -> E.Decl (elabType t (typedefmap, functiondefmap)) v
elabSimp (A.Exp expr) m = E.Exp (elabExpr m expr)

elabLvalue :: A.Lvalue -> ElabState -> E.Lvalue
elabLvalue (A.Variable ident) m = E.Variable ident
elabLvalue (A.LvalueDot lvalue ident) m = E.LvalueDot (elabLvalue lvalue m) ident
elabLvalue (A.LvalueArrow lvalue ident) m =
  E.LvalueDot (E.LvaluePointerStar (elabLvalue lvalue m)) ident
elabLvalue (A.LvaluePointerStar lvalue) m = E.LvaluePointerStar (elabLvalue lvalue m)
elabLvalue (A.LvalueArrayAccess lvalue expr) m = E.LvalueArrayAccess (elabLvalue lvalue m) (elabExpr m expr)

elabLvalueToExpr :: A.Lvalue -> ElabState -> E.Exp
elabLvalueToExpr (A.Variable ident) m = E.Var ident
elabLvalueToExpr (A.LvalueDot lvalue ident) m = E.Dot (elabLvalueToExpr lvalue m) ident
elabLvalueToExpr (A.LvalueArrow lvalue ident) m =
  E.Dot (E.PointerStar (elabLvalueToExpr lvalue m)) ident
elabLvalueToExpr (A.LvaluePointerStar lvalue) m = E.PointerStar (elabLvalueToExpr lvalue m)
elabLvalueToExpr (A.LvalueArrayAccess lvalue expr) m = E.ArrayAccess (elabLvalueToExpr lvalue m) (elabExpr m expr)


elabCtrl :: A.Ctrl -> ElabState -> E.Stmt
elabCtrl (A.If expr stmt A.Eps) m = E.Ctrl (E.If (elabExpr m expr) (elabStmt stmt m) (E.Blk []))
elabCtrl (A.If expr stmt1 (A.Else stmt2)) m = E.Ctrl (E.If (elabExpr m expr) (elabStmt stmt1 m) (elabStmt stmt2 m))
elabCtrl (A.While expr stmt) m = E.Ctrl (E.While (elabExpr m expr) (elabStmt stmt m))
elabCtrl (A.Ret expr) m = E.Ctrl (E.Ret (elabExpr m expr))
elabCtrl (A.RetVoid) _ = E.Ctrl E.RetVoid
elabCtrl (A.Assert expr) m = elabCtrl (A.If (A.Unary Not expr) (A.Blk [A.Simp (A.Exp (A.Call ("" ++ "abort") []))]) A.Eps) m
elabCtrl (A.For s1 expr s2 stmt) m =
  let
    header =
      case s1 of
        A.Epsilon -> []
        A.Simpopt (A.Decl (A.DeclAsgn t v e)) -> 
          [E.Simp (E.Decl (elabType t m) v), E.Simp (E.Asgn (E.Variable v) Equal (elabExpr m e))]
        A.Simpopt s -> [E.Simp (elabSimp s m)]
    footer =
      case s2 of 
        A.Epsilon -> []
        A.Simpopt (A.Decl _) -> error "Declaration in the third position of a for loop"
        A.Simpopt s -> [E.Simp (elabSimp s m)]
    body = E.Blk ([elabStmt stmt m] ++ footer)
  in
    E.Blk (header ++ [E.Ctrl (E.While (elabExpr m expr) body)])

elabExpr :: ElabState -> A.Exp -> E.Exp
elabExpr m (A.Const i) = E.Const i
elabExpr m A.CTrue = E.CTrue
elabExpr m A.CFalse = E.CFalse
elabExpr (typedefmap, functiondefmap) (A.Var ident) = 
  case (Map.member (TypeDefName ident) typedefmap) of
    True -> error("Variable name cannot be the same as a typedef-ed ident!")
    False -> E.Var ident
elabExpr m (A.Unary op e) =
  let
    newe = elabExpr m e
  in
    case newe of
      E.Const a -> precomputeUnary op newe
      E.CTrue -> precomputeUnary op newe
      E.CFalse -> precomputeUnary op newe
      _ -> E.Unary op newe
elabExpr m (A.Binary op e1 e2) = 
  let -- We're setting up for a quick optimization where we precompute constant op constant
    newe1 = elabExpr m e1
    newe2 = elabExpr m e2
  in
    case (newe1, newe2) of 
      (E.Const a, E.Const b) -> precompute op a b
      (E.CTrue, E.CTrue)   -> precomputeLogical op newe1 newe2
      (E.CTrue, E.CFalse)  -> precomputeLogical op newe1 newe2
      (E.CFalse, E.CTrue)  -> precomputeLogical op newe1 newe2
      (E.CFalse, E.CFalse) -> precomputeLogical op newe1 newe2
      _ -> E.Binary op newe1 newe2
elabExpr m (A.Ternary e1 e2 e3) = 
  let
    newe1 = elabExpr m e1
    newe2 = elabExpr m e2
    newe3 = elabExpr m e3
  in
    -- Precompute optimization shifted to unfuckstruct, so we can typecheck
    E.Ternary newe1 newe2 newe3
elabExpr (typedefmap, functiondefmap) (A.Call functionName l) = 
  case ((Map.member functionName functiondefmap) || (functionName == functionPrefix ++ "main")) of
    True -> E.Call functionName (map (elabExpr (typedefmap, functiondefmap)) l)
    False -> error("elabExpr: Call: Function not declared!" ++ show functionName ++ "\n Current map:" ++ show functiondefmap ++ "\n")
elabExpr m (A.Dot expr ident) = (E.Dot (elabExpr m expr) ident)
elabExpr m (A.Arrow expr ident) =
  E.Dot (E.PointerStar (elabExpr m expr)) ident
elabExpr m (A.Alloc ctype) = (E.Alloc (elabType ctype m))
elabExpr m (A.PointerStar expr) = (E.PointerStar (elabExpr m expr))
elabExpr m (A.AllocArray ctype expr) = (E.AllocArray (elabType ctype m) (elabExpr m expr))
elabExpr m (A.ArrayAccess expr1 expr2) = (E.ArrayAccess (elabExpr m expr1) (elabExpr m expr2))
elabExpr m (A.NULL) = (E.NULL)

getFunctionsInExpr :: A.Exp -> [String]
getFunctionsInExpr (A.Call ident l) = ident : (concatMap getFunctionsInExpr l)
getFunctionsInExpr (A.Ternary e1 e2 e3) = (getFunctionsInExpr e1) ++ (getFunctionsInExpr e2) ++ (getFunctionsInExpr e3)
getFunctionsInExpr (A.Binary _ e1 e2) = (getFunctionsInExpr e1) ++ (getFunctionsInExpr e2)
getFunctionsInExpr (A.Unary _ e) = getFunctionsInExpr e
getFunctionsInExpr _ = []

-- Lets us interpret constant-only expressions, saving temps
precompute :: Binop -> Int -> Int -> E.Exp
precompute op aint bint =
  let
    a :: Int32
    a = fromIntegral aint
    b :: Int32
    b = fromIntegral bint
  in
  case op of
    Add -> E.Const $ fromIntegral (a + b)
    Sub -> E.Const $ fromIntegral (a - b)
    Mul -> E.Const $ fromIntegral (a * b)
    Div -> if b /= 0 then E.Const $ fromIntegral (a `handleDiv` b) else E.Binary op (E.Const aint) (E.Const bint)
    Mod -> if b /= 0 then E.Const $ fromIntegral (a `handleMod` b) else E.Binary op (E.Const aint) (E.Const bint)
    Less -> if a < b then E.CTrue else E.CFalse
    Leq -> if a <= b then E.CTrue else E.CFalse
    Geq -> if a >= b then E.CTrue else E.CFalse
    Greater -> if a > b then E.CTrue else E.CFalse
    Eq -> if a == b then E.CTrue else E.CFalse
    Neq -> if a /= b then E.CTrue else E.CFalse
    BAnd -> E.Const $ fromIntegral (a .&. b)
    BXor -> E.Const $ fromIntegral (xor a b)
    BOr -> E.Const $ fromIntegral (a .|. b)
    -- ShiftL -> E.Const $ fromIntegral (shift a bint)   -- Left is positive shift
    -- ShiftR -> E.Const $ fromIntegral (shift a (-bint))  -- Right is positive shift
    _ -> E.Binary op (E.Const aint) (E.Const bint)

-- Haskell is a fucking idiot and tries to round to -infinity
-- C0 rounds towards zero
handleDiv :: Int32 -> Int32 -> Int32
handleDiv a b =
  case (a >= 0, b >= 0) of
    (True, True) -> a `div` b
    (True, False) -> -(a `div` (-b))
    (False, True) -> -((-a) `div` b)
    (False, False) -> (-a) `div` (-b)

-- The default behavior of Haskell with mod is to make everything positive.  We don't want this.
handleMod :: Int32 -> Int32 -> Int32
handleMod n modulus =
  if n < 0 then -((-n) `mod` (abs modulus)) else n `mod` (abs modulus)

precomputeLogical op newe1 newe2 =
  case op of
    LAnd ->
      (case (newe1, newe2) of
        (E.CTrue, E.CTrue)   -> E.CTrue
        (E.CTrue, E.CFalse)  -> E.CFalse
        (E.CFalse, E.CTrue)  -> E.CFalse
        (E.CFalse, E.CFalse) -> E.CFalse
      )
    LOr ->
      (case (newe1, newe2) of
        (E.CTrue, E.CTrue)   -> E.CTrue
        (E.CTrue, E.CFalse)  -> E.CTrue
        (E.CFalse, E.CTrue)  -> E.CTrue
        (E.CFalse, E.CFalse) -> E.CFalse
      )
    Eq ->
      (case (newe1, newe2) of
        (E.CTrue, E.CTrue)   -> E.CTrue
        (E.CTrue, E.CFalse)  -> E.CFalse
        (E.CFalse, E.CTrue)  -> E.CFalse
        (E.CFalse, E.CFalse) -> E.CTrue
      )
    Neq ->
      (case (newe1, newe2) of
        (E.CTrue, E.CTrue)   -> E.CFalse
        (E.CTrue, E.CFalse)  -> E.CTrue
        (E.CFalse, E.CTrue)  -> E.CTrue
        (E.CFalse, E.CFalse) -> E.CFalse
      )

precomputeUnary op e =
  case (op,e) of
    (Neg, E.Const aint) ->
      let
        a :: Int32
        a = fromIntegral aint
      in
        E.Const $ fromIntegral (0 - a)
    (Flip, E.Const aint) ->
      let
        a :: Int64
        a = fromIntegral aint
      in
        E.Const $ fromIntegral (complement a)
    (Not, E.CTrue) -> E.CFalse
    (Not, E.CFalse) -> E.CTrue

--      Add -> E.Const $ (a + b) `mod` modulus
--      Sub -> E.Const $ (a - b) `mod` modulus
--      Mul -> E.Const $ (a * b) `mod` modulus
--      Div -> if b /= 0 then E.Const $ (a `div` b) `mod` modulus else E.Binary op (E.Const a) (E.Const b)
--      Mod -> if b /= 0 then E.Const $ (a `mod` b) `mod` modulus else E.Binary op (E.Const a) (E.Const b)
--      Less -> if a < b then E.CTrue else E.CFalse
--      Leq -> if a <= b then E.CTrue else E.CFalse
--      Geq -> if a >= b then E.CTrue else E.CFalse
--      Greater -> if a > b then E.CTrue else E.CFalse
--      Eq -> if a == b then E.CTrue else E.CFalse
--      Neq -> if a /= b then E.CTrue else E.CFalse
--      _ -> E.Binary op (E.Const a) (E.Const b)

--  case op of
--    Add -> E.Const $ Int.fromIntegral ((Int32.fromIntegral a) + (Int32.fromIntegral b)) 
--    Sub -> E.Const $ Int.fromIntegral ((Int32.fromIntegral a) - (Int32.fromIntegral b)) 
--    Mul -> E.Const $ Int.fromIntegral ((Int32.fromIntegral a) * (Int32.fromIntegral b)) 
--    Div -> if b /= 0 then E.Const $ Int.fromIntegral ((Int32.fromIntegral a) `div` (Int32.fromIntegral b)) else E.Binary op (E.Const a) (E.Const b)
--    Mod -> if b /= 0 then E.Const $ Int.fromIntegral (Int32.fromIntegral (handleMod a b)) else E.Binary op (E.Const a) (E.Const b)
--    Less -> if a < b then E.CTrue else E.CFalse
--    Leq -> if a <= b then E.CTrue else E.CFalse
--    Geq -> if a >= b then E.CTrue else E.CFalse
--    Greater -> if a > b then E.CTrue else E.CFalse
--    Eq -> if a == b then E.CTrue else E.CFalse
--    Neq -> if a /= b then E.CTrue else E.CFalse
--    _ -> E.Binary op (E.Const a) (E.Const b)

-- The default behavior of Haskell with mod is to make everything positive.  We don't want this.
-- handleMod :: Int -> Int -> Int
-- handleMod n modulus =
--   if n < 0 then -((-n) `mod` (abs modulus)) else n `mod` (abs modulus)
-- 
-- doOverflow :: Int -> Int
-- doOverflow n =
--   if n >= (2^31) 
--   then -(2^31) + (n `mod` (2^31))
--   else 
--     if n < -(2^31)
--     then 


