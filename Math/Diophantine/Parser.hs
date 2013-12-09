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
    ( readEquation -- :: String -> Equation
    ) where

import Data.List                 (isInfixOf,find)
import Data.Maybe                (fromMaybe)
import Math.Diophantine.Internal
import Text.Read                 (readMaybe)

data Token = Token { match :: String
                   , value :: Maybe Integer
                   } deriving Show

-- | Reads one token out of the string, returning the value, and the rest of the
-- string.
getToken :: String -> (Token,String)
getToken str = let (t,rs) = span (`notElem` "xy=") str
                in getToken' (t,rs)
  where
      getToken' (t,'x':'y':rs) = (Token (t ++ "xy") $ readT t,rs)
      getToken' (t,v:'^':p:rs) = (Token (t ++ [v,'^',p]) $ readT t,rs)
      getToken' (t,[])       = (Token t $ readT t,[])
      getToken' (t,v:rs)     = (Token (t ++ [v]) $ readT t,rs)
      readT = readMaybe . filter (`notElem`" +")

-- | Builds a list of tokens for the equation.
tokenize :: String -> [Token]
tokenize []  = []
tokenize str = let (t,s) = getToken str
               in t:tokenize s

-- | Reads an 'Equation' as a 'String'. Equations must be set equal to 0.
-- Order of the terms does not matter expect for constant term, and " = 0",
-- which should come last if given.
--
-- >>> readEquation "-5y + 2x - 3xy + 2"
-- -3xy + 2x + -5y + 2 = 0
-- >>> readEquation  "2xy + x + 1 = 0"
-- 2xy + x + 1 = 0
readEquation :: String -> Equation
readEquation str = let ts = tokenize str
                       a = getC ("x^2" `isInfixOf`)               ts
                       b = getC ("xy" `isInfixOf`)                ts
                       c = getC ("y^2" `isInfixOf`)               ts
                       d = getC (\t -> any (== 'x') t
                                       && all (`notElem` "y^") t) ts
                       e = getC (\t -> any (== 'y') t
                                       && all (`notElem` "x^") t) ts
                       f = getC (\t -> all (`notElem` "xy^") t)   ts
                   in GeneralEquation a b c d e f
  where
      getC b ts = fromMaybe 0 $ value $ fromMaybeTok
                  $ find (\(Token s _) -> b s) ts
      fromMaybeTok (Just t@(Token str _))
          = if (filter (`notElem`"+- ") str) `elem` ["x","y","xy","x^2","y^2"]
              then Token str (Just 1)
              else t
      fromMaybeTok Nothing = Token "" (Just 0)
