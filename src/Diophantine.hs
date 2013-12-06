-- |
-- Module      : Math.Diophantine
-- Copyright   : (c) 2009, 2010, 2011, 2012 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts,
--               (c) 2008, 2009 Tom Harper
--
-- License     : GPL v2
-- Maintainer  : joejev@gmail.org
-- Stability   : experimental
-- Portability : GHC
--
-- A module for solving quadratic diophantine equations.


module Math.Diophantine
    ( Equation(..)
    , Solution(..)
    , solve       -- :: Equation -> Solution
    , toMaybeList -- :: Solution -> Maybe [(Integer,Integer)]
    ) where

import Control.Arrow ((***))
import Data.List     ((\\),nub)
import Data.Maybe    (fromMaybe)
import Data.Ratio    ((%),numerator,denominator)

-- -------------------------------------------------------------------------- --
-- data types.

-- | An alias for Integer, used to shorten type signatures.
type Z = Integer

-- | A way to setup an equation in the form of:
--
-- > ax^2 + by^2 + cxy + dx + ey + f = 0
data Equation = GeneralEquation Z Z Z Z Z Z      -- ^ A general quadratic
                                                 -- diophantine equation.
              | LinearEquation Z Z Z             -- ^ dx + ey + f = 0
              | SimpleHyperbolicEquation Z Z Z Z -- ^ by^2 + dx +ey + f = 0
              | ElipticalEquation Z Z Z Z Z Z    -- ^ Eliptical equations.
              | ParabolicEquation Z Z Z Z Z Z    -- ^ Parabolic equations.
                deriving Show

-- | The results of attempting to solve an 'Equation'.
data Solution = AllZ                -- ^ All Integer pairs satisfy the equation.
              | NoSolutions         -- ^ For all (x,y) in ZxZ
              | SolutionSet [(Z,Z)] -- ^ The set of pairs (x,y) that satisfy the
                                    -- equation.

instance Show Solution where
    show  AllZ            = "All Integers"
    show  NoSolutions     = "No Solutions"
    show (SolutionSet ns) = show ns

-- -------------------------------------------------------------------------- --
-- exported functions.

-- | Determines what type of equation to solve for, and then calls the
-- appropriate solve function. Example:
--
-- >>> ghci> solve (GeneralEquation 1 2 3 3 5 0)
-- == [(-3,0),(-2,-1),(0,0),(1,-1)]
solve :: Equation -> Solution
solve (GeneralEquation a b c d e f)
    | a == b && b == c && c == 0 = solveLinear
                                   (LinearEquation d e f)
    | a == c && c == 0 && b /= 0 = solveSimpleHyperbolic
                                   (SimpleHyperbolicEquation b d e f)
    | b^2 - 4 * a * c < 0        = solveEliptical
                                   (ElipticalEquation a b c d e f)
    | b^2 - 4 * a * c == 0       = solveParabolic
                                   (ParabolicEquation a b c d e f)
    | b^2 - 4 * a * c > 0        = error "Not yet implemented"
    | otherwise = error "Unknow Equation type"

-- -------------------------------------------------------------------------- --
-- helper functions.

-- Solves for the Bézout coefficients of a and b.
extended_gcd :: Integral a => a -> a -> (a,a)
extended_gcd a b = extended_gcd' 0 1 b 1 0 a
  where
      extended_gcd' _ _ 0 s' t' _  = (s',t')
      extended_gcd' s t r s' t' r' =
          let q = r' `div` r
          in extended_gcd' (s' - q * s) (t' - q * t) (r' - q * r)  s t r

-- Returns a list of the divisors of n.
divisors :: Integral a => a -> [a]
divisors n =
    n:1:(concat [[x,n `div` x] | x <- [2..floor $ sqrt $ fromIntegral n]
                , n `rem` x == 0]
         \\ [floor $ sqrt $ fromIntegral n | is_square n])
  where
      is_square n = round (sqrt (fromIntegral n)) ^2 == n

-- Extracts the list of solution pairs from a solution.
toMaybeList :: Solution -> Maybe [(Z,Z)]
toMaybeList (SolutionSet ns) = Just ns
toMaybeList _                = Nothing

-- -------------------------------------------------------------------------- --
-- solving functions.

-- | Solves for Equations in the form of dx + ey + f = 0
solveLinear :: Equation -> Solution
solveLinear (LinearEquation d e f)
    | d == 0 && e == 0 = let g     = gcd d e
                             (u,v) = extended_gcd d e
                         in if f == 0
                              then AllZ
                              else solve' d e f g u v
    | d == 0 && e /= 0 = let g     = gcd d e
                             (u,v) = extended_gcd d e
                         in if f `mod` e == 0
                              then solve' d e f g u v
                              else NoSolutions
    | d /= 0 && e /= 0 = let g     = gcd d e
                             (u,v) = extended_gcd d e
                         in if f `mod` g  == 0
                              then solve' d e f g u v
                              else NoSolutions
  where
      solve' d e f g u v = SolutionSet [ s | t <- [0..]
                                       , let x  =   e  *   t  + f * u
                                       , let x' =   e  *   t  - f * u
                                       , let a  =   e  * (-t) + f * u
                                       , let a' =   e  * (-t) - f * u
                                       , let y  = (-d) *   t  + f * v
                                       , let y' = (-d) *   t  - f * v
                                       , let b  = (-d) * (-t) + f * v
                                       , let b' = (-d) * (-t) - f * v
                                       , s <- nub [(x,y),(x',y'),(a,b),(a',b')]
                                       ]
solveLinear _ = error "solveLinear requires a LinearEquation"

-- | Solves for Equations in the form of bxy + dx + ey + f = 0
solveSimpleHyperbolic :: Equation -> Solution
solveSimpleHyperbolic (SimpleHyperbolicEquation b d e f)
    | b == 0 = error "Does not match SimpleHyperbolicEquation form"
    | d * e - b * f == 0 && e `mod` b == 0
        = SolutionSet [e | y <- [0..], e <- nub [ ((-e) `div` b,y)
                                                , ((-e) `div` b,-y)]]
    | d * e - b * f == 0 && d `mod` b == 0
        = SolutionSet [e | x <- [0..], e <- nub [ (x,(-d) `div` b)
                                                , (-x,(-d) `div` b)]]
    | d * e - b * f /= 0
        = SolutionSet $ map (numerator *** numerator)
          $ filter (\(r1,r2) -> denominator r1 == 1 && denominator r2 == 1)
                $ map (\d_i -> ( (d_i - e) % b
                               , ((d * e - b * f) `div` d_i - d) % b))
                $ divisors (d * e - b * f) >>= \n -> [n,-n]
solveSimpleHyperbolic _ =
    error "solveSimpleHyperbolic requires a SimpleHyperbolicEquation"

-- | Solves for Equations in the form of ax^2 + bx^y + cy^2 + dx + ey + f = 0
-- when b^2 - 4ac < 0
solveEliptical :: Equation -> Solution
solveEliptical (ElipticalEquation a b c d e f) =
    if (2 * b * e - 4 * c * d)^2 - 4 * (b^2 - 4 * a * c) * (e^2 - 4 * c * f) > 0
      then let a' = fromIntegral a :: Double
               b' = fromIntegral b :: Double
               c' = fromIntegral c :: Double
               d' = fromIntegral d :: Double
               e' = fromIntegral e :: Double
               f' = fromIntegral f :: Double
               b1 = (-(2 * b' * e' - 4 * c' * d') - sqrt
                     ((2 * b' * e' - 4 * c' * d')^2 - 4 * (b'^2 - 4 * a' * c')
                      * (e'^2 - 4 * c' * f'))) / (2 * (b'^2 - 4 * a' * c'))
               b2 = (-(2 * b' * e' - 4 * c' * d') + sqrt
                     ((2 * b' * e' - 4 * c' * d')^2 - 4 * (b'^2 - 4 * a' * c')
                      * (e'^2 - 4 * c' * f'))) / (2 * (b'^2 - 4 * a' * c'))
               l  = ceiling $ min b1 b2
               u  = floor $ max b1 b2
               cs = [ v | x <- [l..u]
                    , let y'  =  (-(b * x + e) + round
                                  (sqrt (fromIntegral
                                         ((b * x + e)^2
                                          - 4 * c * (a * x^2 + d * x + f)))))
                                 % (2 * c)
                    , let y'' = (-(b * x + e) - round
                                 (sqrt (fromIntegral
                                        ((b * x + e)^2
                                         - 4 * c * (a * x^2 + d * x + f)))))
                                % (2 * c)
                    , v' <- [(x,y'),(x,y'')]
                    , let v = (fst v',numerator $ snd v')
                    , denominator (snd v') == 1
                    ]
               in if null cs
                    then NoSolutions
                    else SolutionSet cs
      else NoSolutions
solveEliptical _ = error "solveEliptical requires ElipticalEquation"

-- | Solves for Equations in the form of ax^2 + bx^y + cy^2 + dx + ey + f = 0
-- when b^2 - 4ac = 0
solveParabolic :: Equation -> Solution
solveParabolic (ParabolicEquation a b c d e f) =
    let g  = if a >= 0
               then abs $ gcd a c
               else - (abs $ gcd a c)
        a' = abs $ a `div` g
        b' =  b `div` g
        c' = abs $ c `div` g
        ra = (round . sqrt . fromIntegral) a'
        rc = if b `div` a >= 0
               then abs $ (round . sqrt . fromIntegral) c'
               else - (abs $ (round . sqrt . fromIntegral) c')
    in if rc * d - ra * e == 0
         then let u_1 = ((-d) + round (sqrt (fromIntegral (d^2 - 4 * a' * g))))
                        `div` (2 * ra)
                  u_2 = ((-d) - round (sqrt (fromIntegral (d^2 - 4 * a' * g))))
                        `div` (2 * ra)
              in SolutionSet $ concat $ zipWith (\a b -> [a,b])
                     (fromMaybe [] $
                      toMaybeList (solveLinear (LinearEquation ra rc (-u_1))))
                     (fromMaybe [] $
                      toMaybeList (solveLinear (LinearEquation ra rc (-u_2))))
         else let us = [u | u <- [0..abs (rc * d - ra * e) - 1]
                       , (ra * g * u^2 + d * u + ra * f)
                        `mod` (rc * d - ra * e) == 0]
              in SolutionSet $ nub
                     [ v | t <- [0..], u <- us
                     , let x1 = rc * g * (ra * e - rc * d) * t^2
                                - (e + 2 * rc * g * u) * t
                                - ((rc * g * u^2 + e * u + rc * f)
                                   `div` (rc * d - ra * e))
                     , let x2 = rc * g * (ra * e - rc * d) * t^2
                                - (e + 2 * rc * g * u) * (-t)
                                - ((rc * g * u^2 + e * u + rc * f)
                                   `div` (rc * d - ra * e))
                     , let y1 = ra * g * (rc * d - ra * e) * t^2
                                + (d + 2 * ra * g * u) * t
                                + ((ra * g * u^2 + d * u + ra * f)
                                   `div` (rc * d - ra * e))
                     , let y2 = ra * g * (rc * d - ra * e) * t^2
                                + (d + 2 * ra * g * u) * (-t)
                                + ((ra * g * u^2 + d * u + ra * f)
                                   `div` (rc * d - ra * e))
                     , v <- [(x1,y1),(x2,y2)]
                     ]
