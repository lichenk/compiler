{-# OPTIONS_GHC -w #-}
module Compile.HeaderParser where

import Compile.Lexer
import Compile.Types.Ops
import Compile.Types.FAST
import Control.Applicative(Applicative(..))

-- parser produced by Happy Version 1.19.4

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11
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

action_0 (22) = happyShift action_7
action_0 (30) = happyShift action_8
action_0 (31) = happyShift action_9
action_0 (32) = happyShift action_10
action_0 (33) = happyShift action_11
action_0 (4) = happyGoto action_2
action_0 (5) = happyGoto action_3
action_0 (6) = happyGoto action_4
action_0 (10) = happyGoto action_5
action_0 (11) = happyGoto action_6
action_0 _ = happyReduce_1

action_1 _ = happyFail

action_2 (58) = happyAccept
action_2 _ = happyFail

action_3 (22) = happyShift action_7
action_3 (30) = happyShift action_8
action_3 (31) = happyShift action_9
action_3 (32) = happyShift action_10
action_3 (33) = happyShift action_11
action_3 (4) = happyGoto action_14
action_3 (5) = happyGoto action_3
action_3 (6) = happyGoto action_4
action_3 (10) = happyGoto action_5
action_3 (11) = happyGoto action_6
action_3 _ = happyReduce_1

action_4 _ = happyReduce_3

action_5 _ = happyReduce_4

action_6 (22) = happyShift action_13
action_6 _ = happyFail

action_7 _ = happyReduce_14

action_8 _ = happyReduce_12

action_9 _ = happyReduce_13

action_10 _ = happyReduce_15

action_11 (22) = happyShift action_7
action_11 (30) = happyShift action_8
action_11 (31) = happyShift action_9
action_11 (32) = happyShift action_10
action_11 (11) = happyGoto action_12
action_11 _ = happyFail

action_12 (22) = happyShift action_17
action_12 _ = happyFail

action_13 (12) = happyShift action_16
action_13 (9) = happyGoto action_15
action_13 _ = happyFail

action_14 _ = happyReduce_2

action_15 (17) = happyShift action_22
action_15 _ = happyFail

action_16 (13) = happyShift action_21
action_16 (22) = happyShift action_7
action_16 (30) = happyShift action_8
action_16 (31) = happyShift action_9
action_16 (32) = happyShift action_10
action_16 (7) = happyGoto action_19
action_16 (11) = happyGoto action_20
action_16 _ = happyFail

action_17 (17) = happyShift action_18
action_17 _ = happyFail

action_18 _ = happyReduce_11

action_19 (16) = happyShift action_25
action_19 (8) = happyGoto action_24
action_19 _ = happyReduce_7

action_20 (22) = happyShift action_23
action_20 _ = happyFail

action_21 _ = happyReduce_9

action_22 _ = happyReduce_5

action_23 _ = happyReduce_6

action_24 (13) = happyShift action_27
action_24 _ = happyFail

action_25 (22) = happyShift action_7
action_25 (30) = happyShift action_8
action_25 (31) = happyShift action_9
action_25 (32) = happyShift action_10
action_25 (7) = happyGoto action_26
action_25 (11) = happyGoto action_20
action_25 _ = happyFail

action_26 (16) = happyShift action_25
action_26 (8) = happyGoto action_28
action_26 _ = happyReduce_7

action_27 _ = happyReduce_10

action_28 _ = happyReduce_8

happyReduce_1 = happySpecReduce_0  4 happyReduction_1
happyReduction_1  =  HappyAbsSyn4
		 ([]
	)

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happyReduce 4 6 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	(HappyTerminal (TokIdent happy_var_2)) `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Fdecl happy_var_1 happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_2  7 happyReduction_6
happyReduction_6 (HappyTerminal (TokIdent happy_var_2))
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn7
		 (Param happy_var_1 happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  8 happyReduction_7
happyReduction_7  =  HappyAbsSyn8
		 ([]
	)

happyReduce_8 = happySpecReduce_3  8 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_3)
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2 : happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  9 happyReduction_9
happyReduction_9 _
	_
	 =  HappyAbsSyn9
		 ([]
	)

happyReduce_10 = happyReduce 4 9 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (happy_var_2 : happy_var_3
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 4 10 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyTerminal (TokIdent happy_var_3)) `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (Typedef happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_1  11 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn11
		 (CInt
	)

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn11
		 (CBool
	)

happyReduce_14 = happySpecReduce_1  11 happyReduction_14
happyReduction_14 (HappyTerminal (TokIdent happy_var_1))
	 =  HappyAbsSyn11
		 (CTypeIdent happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  11 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn11
		 (CVoid
	)

happyNewToken action sts stk [] =
	action 58 58 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokLParen -> cont 12;
	TokRParen -> cont 13;
	TokLBrace -> cont 14;
	TokRBrace -> cont 15;
	TokComma -> cont 16;
	TokSemi -> cont 17;
	TokColon -> cont 18;
	TokQuestion -> cont 19;
	TokDec happy_dollar_dollar -> cont 20;
	TokHex happy_dollar_dollar -> cont 21;
	TokIdent happy_dollar_dollar -> cont 22;
	TokIf -> cont 23;
	TokElse -> cont 24;
	TokWhile -> cont 25;
	TokFor -> cont 26;
	TokReturn -> cont 27;
	TokTrue -> cont 28;
	TokFalse -> cont 29;
	TokInt -> cont 30;
	TokBool -> cont 31;
	TokVoid -> cont 32;
	TokTypedef -> cont 33;
	TokMinus -> cont 34;
	TokNot -> cont 35;
	TokFlip -> cont 36;
	TokPlus -> cont 37;
	TokTimes -> cont 38;
	TokDiv -> cont 39;
	TokMod -> cont 40;
	TokLShift -> cont 41;
	TokRShift -> cont 42;
	TokLess -> cont 43;
	TokGreater -> cont 44;
	TokGeq -> cont 45;
	TokLeq -> cont 46;
	TokNeq -> cont 47;
	TokBAnd -> cont 48;
	TokBXor -> cont 49;
	TokBOr -> cont 50;
	TokLAnd -> cont 51;
	TokLOr -> cont 52;
	TokEquality -> cont 53;
	TokAsgnop happy_dollar_dollar -> cont 54;
	TokPostop happy_dollar_dollar -> cont 55;
	TokAssert -> cont 56;
	TokReserved -> cont 57;
	_ -> happyError' (tk:tks)
	}

happyError_ 58 tk tks = happyError' tks
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
parseError t = error ("Parse Error: " ++ show t)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

























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

