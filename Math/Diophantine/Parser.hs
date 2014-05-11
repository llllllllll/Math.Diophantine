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

import Math.Diophantine.Internal
import Math.Diophantine.Grammar

-- | Reads an equation from a string returning an 'Equation' or 'ParseError'.
readEquation :: String -> Either ParseError Equation
readEquation cs = case parseRawEquation cs of
                      Invalid pe -> Left pe
                      Valid eq   -> Right $ fromRawEq eq

fromRawEq :: Equals -> Equation
fromRawEq (Equals lh rh) = undefined
