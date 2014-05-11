{-# OPTIONS_GHC -w #-}
module Math.Diophantine.Grammar
    ( parseRawEquation -- :: String -> EqParser Equals
    ) where

import Data.Char (isDigit,isSpace)

-- parser produced by Happy Version 1.19.3

data HappyAbsSyn t4 t5 t6 t7
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7

action_0 (8) = happyShift action_5
action_0 (9) = happyShift action_6
action_0 (10) = happyShift action_7
action_0 (11) = happyShift action_8
action_0 (16) = happyShift action_9
action_0 (18) = happyShift action_10
action_0 (4) = happyGoto action_11
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 _ = happyFail

action_1 (8) = happyShift action_5
action_1 (9) = happyShift action_6
action_1 (10) = happyShift action_7
action_1 (11) = happyShift action_8
action_1 (16) = happyShift action_9
action_1 (18) = happyShift action_10
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 _ = happyFail

action_2 (12) = happyShift action_18
action_2 (13) = happyShift action_19
action_2 (14) = happyShift action_20
action_2 _ = happyFail

action_3 _ = happyReduce_4

action_4 (15) = happyShift action_17
action_4 _ = happyReduce_6

action_5 _ = happyReduce_10

action_6 _ = happyReduce_11

action_7 _ = happyReduce_12

action_8 _ = happyReduce_13

action_9 (8) = happyShift action_13
action_9 (9) = happyShift action_14
action_9 (10) = happyShift action_15
action_9 (11) = happyShift action_16
action_9 _ = happyFail

action_10 (8) = happyShift action_5
action_10 (9) = happyShift action_6
action_10 (10) = happyShift action_7
action_10 (11) = happyShift action_8
action_10 (16) = happyShift action_9
action_10 (7) = happyGoto action_12
action_10 _ = happyReduce_5

action_11 (19) = happyAccept
action_11 _ = happyFail

action_12 (15) = happyShift action_33
action_12 _ = happyReduce_8

action_13 (10) = happyShift action_31
action_13 (11) = happyShift action_32
action_13 _ = happyFail

action_14 (10) = happyShift action_29
action_14 (11) = happyShift action_30
action_14 _ = happyFail

action_15 (8) = happyShift action_27
action_15 (9) = happyShift action_28
action_15 _ = happyFail

action_16 (8) = happyShift action_25
action_16 (9) = happyShift action_26
action_16 _ = happyFail

action_17 (18) = happyShift action_24
action_17 _ = happyFail

action_18 (8) = happyShift action_5
action_18 (9) = happyShift action_6
action_18 (10) = happyShift action_7
action_18 (11) = happyShift action_8
action_18 (16) = happyShift action_9
action_18 (18) = happyShift action_10
action_18 (5) = happyGoto action_23
action_18 (6) = happyGoto action_3
action_18 (7) = happyGoto action_4
action_18 _ = happyFail

action_19 (8) = happyShift action_5
action_19 (9) = happyShift action_6
action_19 (10) = happyShift action_7
action_19 (11) = happyShift action_8
action_19 (16) = happyShift action_9
action_19 (18) = happyShift action_10
action_19 (6) = happyGoto action_22
action_19 (7) = happyGoto action_4
action_19 _ = happyFail

action_20 (8) = happyShift action_5
action_20 (9) = happyShift action_6
action_20 (10) = happyShift action_7
action_20 (11) = happyShift action_8
action_20 (16) = happyShift action_9
action_20 (18) = happyShift action_10
action_20 (6) = happyGoto action_21
action_20 (7) = happyGoto action_4
action_20 _ = happyFail

action_21 _ = happyReduce_3

action_22 _ = happyReduce_2

action_23 (13) = happyShift action_19
action_23 (14) = happyShift action_20
action_23 _ = happyReduce_1

action_24 _ = happyReduce_7

action_25 (17) = happyShift action_42
action_25 _ = happyFail

action_26 (17) = happyShift action_41
action_26 _ = happyFail

action_27 (17) = happyShift action_40
action_27 _ = happyFail

action_28 (17) = happyShift action_39
action_28 _ = happyFail

action_29 (17) = happyShift action_38
action_29 _ = happyFail

action_30 (17) = happyShift action_37
action_30 _ = happyFail

action_31 (17) = happyShift action_36
action_31 _ = happyFail

action_32 (17) = happyShift action_35
action_32 _ = happyFail

action_33 (18) = happyShift action_34
action_33 _ = happyFail

action_34 _ = happyReduce_9

action_35 _ = happyReduce_16

action_36 _ = happyReduce_14

action_37 _ = happyReduce_17

action_38 _ = happyReduce_15

action_39 _ = happyReduce_20

action_40 _ = happyReduce_18

action_41 _ = happyReduce_21

action_42 _ = happyReduce_19

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Equals happy_var_1 happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (ETerm happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn6
		 (Constant happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (Variable 1 happy_var_1 1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happyMonadReduce 3 6 happyReduction_7
happyReduction_7 ((HappyTerminal (TokenInt happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( if happy_var_3 `notElem` [1,2]
                                     then Invalid PowerOutOfBounds
                                     else return $ Variable 1 happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_8 = happySpecReduce_2  6 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn6
		 (Variable happy_var_1 happy_var_2 1
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happyMonadReduce 4 6 happyReduction_9
happyReduction_9 ((HappyTerminal (TokenInt happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	(HappyTerminal (TokenInt happy_var_1)) `HappyStk`
	happyRest) tk
	 = happyThen (( if happy_var_4 `notElem` [1,2]
                                     then Invalid PowerOutOfBounds
                                     else return $ Variable happy_var_1 happy_var_2 happy_var_4)
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_10 = happySpecReduce_1  7 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn7
		 (XTerm
	)

happyReduce_11 = happySpecReduce_1  7 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn7
		 (XTerm
	)

happyReduce_12 = happySpecReduce_1  7 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn7
		 (YTerm
	)

happyReduce_13 = happySpecReduce_1  7 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn7
		 (YTerm
	)

happyReduce_14 = happyReduce 4 7 happyReduction_14
happyReduction_14 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (XYTerm
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 4 7 happyReduction_15
happyReduction_15 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (XYTerm
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 4 7 happyReduction_16
happyReduction_16 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (XYTerm
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 4 7 happyReduction_17
happyReduction_17 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (XYTerm
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 4 7 happyReduction_18
happyReduction_18 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (XYTerm
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 4 7 happyReduction_19
happyReduction_19 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (XYTerm
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 4 7 happyReduction_20
happyReduction_20 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (XYTerm
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 4 7 happyReduction_21
happyReduction_21 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (XYTerm
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 19 19 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenX -> cont 8;
	TokenX -> cont 9;
	TokenY -> cont 10;
	TokenY -> cont 11;
	TokenEq -> cont 12;
	TokenPlus -> cont 13;
	TokenMinus -> cont 14;
	TokenExp -> cont 15;
	TokenO -> cont 16;
	TokenC -> cont 17;
	TokenInt happy_dollar_dollar -> cont 18;
	_ -> happyError' (tk:tks)
	}

happyError_ 19 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => EqParser a -> (a -> EqParser b) -> EqParser b
happyThen = (>>=)
happyReturn :: () => a -> EqParser a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> EqParser a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> EqParser a
happyError' = parseError

parseRawEquation tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-- | Function to invoke in case of an error.
parseError :: [Token] -> EqParser a
parseError _ = Invalid BadGrammar

-- | The equation parsing monad.
data EqParser a = Valid a
                | Invalid ParseError
                  deriving Show

-- | The types of parse erros that can occur.
data ParseError = PowerOutOfBounds -- ^ We are only solving quadratics.
                | BadGrammar       -- ^ Not a valid equation type.
                  deriving Show

-- | Monad instance for the parser.
instance Monad EqParser where
    return t            = Valid t
    (>>=) (Valid v) f   = f v
    (>>=) (Invalid i) _ = Invalid i

-- | The main equality statement.
data Equals = Equals Expr Expr deriving Show

-- | An expression that lives on one side of an equality statement.
data Expr = Plus Expr Term   -- ^ Addition.
          | Minus Expr Term  -- ^ Subtraction.
          | ETerm Term       -- ^ A single expression as a term.
            deriving Show

data Term = Constant Int              -- ^ Constant terms.
          | Variable Int VarTerm Int  -- ^ Terms with variables.
            deriving Show

data VarTerm = XTerm  -- ^ Terms with x.
             | YTerm  -- ^ Terms with y.
             | XYTerm -- ^ terms with both x and y.
               deriving Show

-- | The token types.
data Token = TokenX       -- ^ 'x' | 'X'
           | TokenY       -- ^ 'y' | 'X'
           | TokenEq      -- ^ '='
           | TokenPlus    -- ^ '+'
           | TokenMinus   -- ^ '-'
           | TokenExp     -- ^ '^'
           | TokenInt Int -- ^ Integers
           | TokenO       -- ^ '('
           | TokenC       -- ^ ')'
             deriving Show

-- | A basic lexing function.
lexer :: String -> [Token]
lexer [] = []
lexer str@(c:cs)
    | isSpace c = lexer cs
    | isDigit c = lexNum str
lexer ('x':cs) = TokenX     : lexer cs
lexer ('X':cs) = TokenX     : lexer cs
lexer ('y':cs) = TokenY     : lexer cs
lexer ('Y':cs) = TokenY     : lexer cs
lexer ('=':cs) = TokenEq    : lexer cs
lexer ('+':cs) = TokenPlus  : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('^':cs) = TokenExp   : lexer cs
lexer ('(':cs) = TokenO     : lexer cs
lexer (')':cs) = TokenC     : lexer cs

-- | Lexes the 'TokenInt's.
lexNum :: String -> [Token]
lexNum cs = let (num,rest) = span isDigit cs
            in TokenInt (read num) : lexer rest
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4










































{-# LINE 8 "<command-line>" #-}
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
