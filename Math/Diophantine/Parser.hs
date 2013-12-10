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
    ( ReadError(..)     -- instances: Show
    , readEquation      -- :: String -> Either ReadError Equation
    , readMaybeEquation -- :: String -> Maybe Equation
    ) where

import Data.List                 (isInfixOf,find)
import Data.Maybe                (fromMaybe,isNothing)
import Math.Diophantine.Internal
import Text.Read                 (readMaybe)

-- -------------------------------------------------------------------------- --
-- Data.

-- | A type representing a token to be read.
data Token = Token { match :: String
                   , value :: Either ReadError Integer
                   } deriving Show

-- | A type to represent any errors that may occur in reading a the equation.
data ReadError = UnexpectedChar Char deriving Show

-- -------------------------------------------------------------------------- --
-- Internal parsing functions.

-- | Reads one token out of the string, returning the value, and the rest of the
-- string.
getToken :: String -> (Token,String)
getToken str = let (t,rs) = span (`notElem` "xy=") str
                in getToken' (t,rs)
  where
      getToken' (t,'x':'y':rs) = (Token "xy" $ readT t,rs)
      getToken' (t,v:'^':p:rs) = (Token [v,'^',p] $ readT t,rs)
      getToken' (t,[])       = (Token "" $ readT t,[])
      getToken' (t,v:rs)     = (Token [v] $ readT t,rs)
      readT = readWithError . filter (`notElem`" +")

-- | Builds a list of tokens for the equation.
tokenize :: String -> [Token]
tokenize []  = []
tokenize str = let (t,s) = getToken str
               in t:tokenize s

-- | A way to make verify that the 'String's are proper, and also report any
-- 'ReadError's.
readWithError :: String -> Either ReadError Integer
readWithError ""  = Right 1
readWithError str
    | null $ filter (`elem` "0123456789-") str = Right (-1)
    | otherwise = case find (`notElem` "0123456789-") str of
                      Nothing -> Right $ read str
                      Just c  -> Left $ UnexpectedChar c

-- | WARNING: Throws error: extracts the 'Integer' value from the value of a
-- token. Only used after it is assured it is NOT a 'ReadError'.
getVal :: Either ReadError Integer -> Integer
getVal (Left _)  = error "Parsing error when filtering errors."
getVal (Right n) = n

-- | WARNING: Throws error: extracts the 'ReadError' value from the value of a
-- token. Only used after it is assured it is NOT an 'Integer'.
getErr :: Either ReadError Integer -> ReadError
getErr (Right _) = error "Parse error when finding errors."
getErr (Left e)  = e

-- | Gets the coefficient for the given term out of the list of tokens.
getC :: (String -> Bool) -> [Token] -> Either ReadError Integer
getC b ts = value $ fromMaybeTok $ find (\t -> b $ match t) ts

-- | a fromMaybe specialized to 'Token's.
fromMaybeTok :: Maybe Token -> Token
fromMaybeTok (Just (Token s v)) = Token s v
fromMaybeTok Nothing            = Token "" (Right 0)

-- -------------------------------------------------------------------------- --
-- Exported functions.

-- | Reads an 'Equation' as a 'String'. Equations must be set equal to 0.
-- Order of the terms does not matter expect for constant term, and " = 0",
-- which should come last if given. Returns 'Either ReadError Equation'
--
-- >>> readEquation "-5y + 2x - 3xy + 2"
-- -3xy + 2x + -5y + 2 = 0
-- >>> readEquation  "2xy + x + 1 = 0"
-- 2xy + x + 1 = 0
readEquation :: String -> Either ReadError Equation
readEquation str = let ts = tokenize str
                       a  = getC ("x^2" ==) ts
                       b  = getC ("xy"  ==) ts
                       c  = getC ("y^2" ==) ts
                       d  = getC ("x"   ==) ts
                       e  = getC ("y"   ==) ts
                       f  = getC null       ts
                       g  = find (\n -> case n of
                                            Left _ -> True
                                            _      -> False) [a,b,c,d,e,f]
                   in maybe (Right $ GeneralEquation (getVal a) (getVal b)
                             (getVal c) (getVal d) (getVal e) (getVal f))
                          (Left . getErr) g

-- | Like readEquation; however, it does not return any errors.
readMaybeEquation :: String -> Maybe Equation
readMaybeEquation str = let ts = tokenize str
                            a  =  getC ("x^2" ==) ts
                            b  = getC ("xy"  ==) ts
                            c  = getC ("y^2" ==) ts
                            d  = getC ("x"   ==) ts
                            e  = getC ("y"   ==) ts
                            f  = getC null       ts
                            g  = find (\n -> case n of
                                                 Left _ -> True
                                                 _      -> False) [a,b,c,d,e,f]
                        in if isNothing g
                             then Just $ GeneralEquation (getVal a) (getVal b)
                                      (getVal c) (getVal d) (getVal e)
                                                     (getVal f)
                             else Nothing
