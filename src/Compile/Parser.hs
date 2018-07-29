{-# OPTIONS_GHC -w #-}
module Compile.Parser where

import Compile.Lexer
import Compile.Types.Ops
import Compile.Types.FAST
import Control.Applicative(Applicative(..))

-- parser produced by Happy Version 1.19.4

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14

action_0 (29) = happyShift action_13
action_0 (32) = happyShift action_5
action_0 (40) = happyShift action_6
action_0 (41) = happyShift action_7
action_0 (42) = happyShift action_8
action_0 (43) = happyShift action_14
action_0 (4) = happyGoto action_9
action_0 (5) = happyGoto action_2
action_0 (9) = happyGoto action_10
action_0 (10) = happyGoto action_11
action_0 (11) = happyGoto action_12
action_0 (14) = happyGoto action_3
action_0 _ = happyFail

action_1 (29) = happyShift action_4
action_1 (32) = happyShift action_5
action_1 (40) = happyShift action_6
action_1 (41) = happyShift action_7
action_1 (42) = happyShift action_8
action_1 (5) = happyGoto action_2
action_1 (14) = happyGoto action_3
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (15) = happyShift action_18
action_3 (32) = happyShift action_19
action_3 (48) = happyShift action_20
action_3 _ = happyFail

action_4 (32) = happyShift action_17
action_4 _ = happyFail

action_5 _ = happyReduce_19

action_6 _ = happyReduce_17

action_7 _ = happyReduce_18

action_8 _ = happyReduce_20

action_9 (69) = happyAccept
action_9 _ = happyFail

action_10 _ = happyReduce_2

action_11 _ = happyReduce_3

action_12 _ = happyReduce_4

action_13 (32) = happyShift action_16
action_13 _ = happyFail

action_14 (29) = happyShift action_4
action_14 (32) = happyShift action_5
action_14 (40) = happyShift action_6
action_14 (41) = happyShift action_7
action_14 (42) = happyShift action_8
action_14 (14) = happyGoto action_15
action_14 _ = happyFail

action_15 (15) = happyShift action_18
action_15 (32) = happyShift action_26
action_15 (48) = happyShift action_20
action_15 _ = happyFail

action_16 (19) = happyShift action_24
action_16 (22) = happyShift action_25
action_16 _ = happyReduce_23

action_17 _ = happyReduce_23

action_18 (16) = happyShift action_23
action_18 _ = happyFail

action_19 (17) = happyShift action_22
action_19 (8) = happyGoto action_21
action_19 _ = happyFail

action_20 _ = happyReduce_21

action_21 (22) = happyShift action_34
action_21 _ = happyFail

action_22 (18) = happyShift action_33
action_22 (29) = happyShift action_4
action_22 (32) = happyShift action_5
action_22 (40) = happyShift action_6
action_22 (41) = happyShift action_7
action_22 (42) = happyShift action_8
action_22 (6) = happyGoto action_31
action_22 (14) = happyGoto action_32
action_22 _ = happyFail

action_23 _ = happyReduce_22

action_24 (29) = happyShift action_4
action_24 (32) = happyShift action_5
action_24 (40) = happyShift action_6
action_24 (41) = happyShift action_7
action_24 (42) = happyShift action_8
action_24 (12) = happyGoto action_28
action_24 (13) = happyGoto action_29
action_24 (14) = happyGoto action_30
action_24 _ = happyReduce_15

action_25 _ = happyReduce_12

action_26 (22) = happyShift action_27
action_26 _ = happyFail

action_27 _ = happyReduce_11

action_28 (29) = happyShift action_4
action_28 (32) = happyShift action_5
action_28 (40) = happyShift action_6
action_28 (41) = happyShift action_7
action_28 (42) = happyShift action_8
action_28 (12) = happyGoto action_28
action_28 (13) = happyGoto action_40
action_28 (14) = happyGoto action_30
action_28 _ = happyReduce_15

action_29 (20) = happyShift action_39
action_29 _ = happyFail

action_30 (15) = happyShift action_18
action_30 (32) = happyShift action_38
action_30 (48) = happyShift action_20
action_30 _ = happyFail

action_31 (21) = happyShift action_37
action_31 (7) = happyGoto action_36
action_31 _ = happyReduce_7

action_32 (15) = happyShift action_18
action_32 (32) = happyShift action_35
action_32 (48) = happyShift action_20
action_32 _ = happyFail

action_33 _ = happyReduce_9

action_34 _ = happyReduce_5

action_35 _ = happyReduce_6

action_36 (18) = happyShift action_44
action_36 _ = happyFail

action_37 (29) = happyShift action_4
action_37 (32) = happyShift action_5
action_37 (40) = happyShift action_6
action_37 (41) = happyShift action_7
action_37 (42) = happyShift action_8
action_37 (6) = happyGoto action_43
action_37 (14) = happyGoto action_32
action_37 _ = happyFail

action_38 (22) = happyShift action_42
action_38 _ = happyFail

action_39 (22) = happyShift action_41
action_39 _ = happyFail

action_40 _ = happyReduce_16

action_41 _ = happyReduce_13

action_42 _ = happyReduce_14

action_43 (21) = happyShift action_37
action_43 (7) = happyGoto action_45
action_43 _ = happyReduce_7

action_44 _ = happyReduce_10

action_45 _ = happyReduce_8

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happyReduce 4 5 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyTerminal (TokIdent happy_var_2)) `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Fdecl happy_var_1 happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_2  6 happyReduction_6
happyReduction_6 (HappyTerminal (TokIdent happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn6
		 (Param happy_var_1 happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  7 happyReduction_7
happyReduction_7  =  HappyAbsSyn7
		 ([]
	)

happyReduce_8 = happySpecReduce_3  7 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_3)
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2 : happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  8 happyReduction_9
happyReduction_9 _
	_
	 =  HappyAbsSyn8
		 ([]
	)

happyReduce_10 = happyReduce 4 8 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (happy_var_2 : happy_var_3
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 4 9 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyTerminal (TokIdent happy_var_3)) `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Typedef happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_3  10 happyReduction_12
happyReduction_12 _
	(HappyTerminal (TokIdent happy_var_2))
	_
	 =  HappyAbsSyn10
		 (Sdecl happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 6 11 happyReduction_13
happyReduction_13 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokIdent happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (Sdef happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_3  12 happyReduction_14
happyReduction_14 _
	(HappyTerminal (TokIdent happy_var_2))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn12
		 (Field happy_var_1 happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_0  13 happyReduction_15
happyReduction_15  =  HappyAbsSyn13
		 ([]
	)

happyReduce_16 = happySpecReduce_2  13 happyReduction_16
happyReduction_16 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  14 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn14
		 (CInt
	)

happyReduce_18 = happySpecReduce_1  14 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn14
		 (CBool
	)

happyReduce_19 = happySpecReduce_1  14 happyReduction_19
happyReduction_19 (HappyTerminal (TokIdent happy_var_1))
	 =  HappyAbsSyn14
		 (CTypeIdent happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  14 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn14
		 (CVoid
	)

happyReduce_21 = happySpecReduce_2  14 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (CPtr happy_var_1
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  14 happyReduction_22
happyReduction_22 _
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (CArray happy_var_1
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  14 happyReduction_23
happyReduction_23 (HappyTerminal (TokIdent happy_var_2))
	_
	 =  HappyAbsSyn14
		 (CStruct happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 69 69 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokLSquare -> cont 15;
	TokRSquare -> cont 16;
	TokLParen -> cont 17;
	TokRParen -> cont 18;
	TokLBrace -> cont 19;
	TokRBrace -> cont 20;
	TokComma -> cont 21;
	TokSemi -> cont 22;
	TokColon -> cont 23;
	TokQuestion -> cont 24;
	TokDot -> cont 25;
	TokArrow -> cont 26;
	TokAlloc -> cont 27;
	TokAllocArr -> cont 28;
	TokStruct -> cont 29;
	TokDec happy_dollar_dollar -> cont 30;
	TokHex happy_dollar_dollar -> cont 31;
	TokIdent happy_dollar_dollar -> cont 32;
	TokIf -> cont 33;
	TokElse -> cont 34;
	TokWhile -> cont 35;
	TokFor -> cont 36;
	TokReturn -> cont 37;
	TokTrue -> cont 38;
	TokFalse -> cont 39;
	TokInt -> cont 40;
	TokBool -> cont 41;
	TokVoid -> cont 42;
	TokTypedef -> cont 43;
	TokMinus -> cont 44;
	TokNot -> cont 45;
	TokFlip -> cont 46;
	TokPlus -> cont 47;
	TokTimes -> cont 48;
	TokDiv -> cont 49;
	TokMod -> cont 50;
	TokLShift -> cont 51;
	TokRShift -> cont 52;
	TokLess -> cont 53;
	TokGreater -> cont 54;
	TokGeq -> cont 55;
	TokLeq -> cont 56;
	TokNeq -> cont 57;
	TokBAnd -> cont 58;
	TokBXor -> cont 59;
	TokBOr -> cont 60;
	TokLAnd -> cont 61;
	TokLOr -> cont 62;
	TokEquality -> cont 63;
	TokAsgnop happy_dollar_dollar -> cont 64;
	TokPostop happy_dollar_dollar -> cont 65;
	TokAssert -> cont 66;
	TokNULL -> cont 67;
	TokReserved -> cont 68;
	_ -> happyError' (tk:tks)
	}

happyError_ 69 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure    = return
    a <*> b = (fmap id a) <*> b
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parseTokens tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError t = error ("ParseOther Error: " ++ show t)

convertToLvalue :: Exp -> Lvalue
convertToLvalue (Var ident) = Variable ident
convertToLvalue (Dot lvalue ident) = LvalueDot (convertToLvalue lvalue) ident
convertToLvalue (Arrow lvalue ident) = LvalueArrow (convertToLvalue lvalue) ident
convertToLvalue (PointerStar lvalue) = LvaluePointerStar (convertToLvalue lvalue)
convertToLvalue (ArrayAccess lvalue expr) =
  LvalueArrayAccess (convertToLvalue lvalue) expr
convertToLvalue other = error("Not an lvalue:" ++ show other)

formAsgn :: Exp -> Asnop -> Exp -> Simp
formAsgn e1 op e2 =
  Asgn (convertToLvalue e1) op e2

formPost :: Exp -> Postop -> Simp
formPost e1 op =
  Post (convertToLvalue e1) op

checkDeclAsgn :: CType -> Ident -> Asnop -> Exp -> Decl
checkDeclAsgn t v op e =
  case op of 
    Equal -> DeclAsgn t v e
    _ -> error "Invalid assignment operator on a declaration"

checkDec n = if (n > 2^31) then error "Decimal too big" else Const n
checkHex n = if (n >= 2^32) then error "Hex too big" else Const n
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
