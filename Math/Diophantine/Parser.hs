-- |
-- Module      : Math.Diophantine.Parser
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- A module for parsing 'Equation's.

module Math.Diophantine.Parser
    ( ParseError(..)    -- instances: Show
    , readEquation      -- :: String -> Either ParseError Equation
    , readMaybeEquation -- :: String -> Maybe Equation
    ) where

import Math.Diophantine.Internal (Equation(..), Z(..))
import Math.Diophantine.Grammar ( EqParser(..)
                                , ParseError(..)
                                , parseRawEquation
                                , Equals(..)
                                , Expr(..)
                                , Term(..)
                                , VarTerm(..)
                                )


-- | Reads an equation from a string returning an 'Equation' or 'ParseError'.
readEquation :: String -> Either ParseError Equation
readEquation cs = case parseRawEquation cs of
                      Invalid pe -> Left pe
                      Valid eq   -> Right $ fromRawEq eq


-- | like 'readEquation' but discards the ParseError's data and only returns
-- a valid 'Equation' or 'Nothing'.
readMaybeEquation :: String -> Maybe Equation
readMaybeEquation cs = case readEquation cs of
                           Left _  -> Nothing
                           Right e -> Just e


-- | Moves all the expressions to the lhs of the eq and simplifies.
fromRawEq :: Equals -> Equation
fromRawEq (Equals lhs rhs) = let lx  = findXCoef    lhs 0
                                 ly  = findYCoef    lhs 0
                                 lx2 = findX2Coef   lhs 0
                                 ly2 = findY2Coef   lhs 0
                                 lxy = findXYCoef   lhs 0
                                 lc  = findConstant lhs 0
                                 rx  = findXCoef    rhs 0
                                 ry  = findYCoef    rhs 0
                                 rx2 = findX2Coef   rhs 0
                                 ry2 = findY2Coef   rhs 0
                                 rxy = findXYCoef   rhs 0
                                 rc  = findConstant rhs 0
                             in GeneralEquation
                                    (lx2 - rx2)
                                    (lxy - rxy)
                                    (ly2 - ry2)
                                    (lx  - rx)
                                    (ly  - ry)
                                    (lc  - rc)


-- -------------------------------------------------------------------------- --
-- Find the coefficients for the different terms
-- TODO: Make this in template haskell ;_;


findXCoef :: Expr -> Z -> Z
findXCoef (Plus expr (Variable n XTerm 1))  m = findXCoef expr (n + m)
findXCoef (Plus expr _)                     m = findXCoef expr m
findXCoef (Minus expr (Variable n XTerm 1)) m = findXCoef expr (m - n)
findXCoef (Minus expr _)                    m = findXCoef expr m
findXCoef (ETerm (Variable n XTerm 1))      m = n + m
findXCoef (ETerm _)                         m = m


findX2Coef :: Expr -> Z -> Z
findX2Coef (Plus expr (Variable n XTerm 2))  m = findX2Coef expr (n + m)
findX2Coef (Plus expr _)                     m = findX2Coef expr m
findX2Coef (Minus expr (Variable n XTerm 2)) m = findX2Coef expr (m - n)
findX2Coef (Minus expr _)                    m = findX2Coef expr m
findX2Coef (ETerm (Variable n XTerm 2))      m = n + m
findX2Coef (ETerm _)                         m = m


findYCoef :: Expr -> Z -> Z
findYCoef (Plus expr (Variable n YTerm 1))  m = findYCoef expr (n + m)
findYCoef (Plus expr _)                     m = findYCoef expr m
findYCoef (Minus expr (Variable n YTerm 1)) m = findYCoef expr (m - n)
findYCoef (Minus expr _)                    m = findYCoef expr m
findYCoef (ETerm (Variable n YTerm 1))      m = n + m
findYCoef (ETerm _)                         m = m


findY2Coef :: Expr -> Z -> Z
findY2Coef (Plus expr (Variable n YTerm 2))  m = findY2Coef expr (n + m)
findY2Coef (Plus expr _)                     m = findY2Coef expr m
findY2Coef (Minus expr (Variable n YTerm 2)) m = findY2Coef expr (m - n)
findY2Coef (Minus expr _)                    m = findY2Coef expr m
findY2Coef (ETerm (Variable n YTerm 2))      m = n + m
findY2Coef (ETerm _)                         m = m


findXYCoef :: Expr -> Z -> Z
findXYCoef (Plus expr (Variable n XYTerm 1))  m = findXYCoef expr (n + m)
findXYCoef (Plus expr _)                      m = findXYCoef expr m
findXYCoef (Minus expr (Variable n XYTerm 1)) m = findXYCoef expr (m - n)
findXYCoef (Minus expr _)                     m = findXYCoef expr m
findXYCoef (ETerm (Variable n XYTerm 1))      m = n + m
findXYCoef (ETerm _)                          m = m


findConstant :: Expr -> Z -> Z
findConstant (Plus expr (Constant n))  m = findConstant expr (n + m)
findConstant (Plus expr _)             m = findConstant expr m
findConstant (Minus expr (Constant n)) m = findConstant expr (m - n)
findConstant (Minus expr _)            m = findConstant expr m
findConstant (ETerm (Constant n))      m = n + m
findConstant (ETerm _)                 m = m
