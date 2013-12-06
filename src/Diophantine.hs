-- |
-- Module      : Math.Diophantine
-- Copyright   : 2013 Joe Jevnik
--
-- License     : GPL v2
-- Maintainer  : joejev@gmail.org
-- Stability   : experimental
-- Portability : GHC
--
-- A module for solving quadratic diophantine equations.


module Math.Diophantine
    ( Equation(..)          -- instance of: Show
    , Solution(..)          -- instance of: Eq, Show
    , specializeEquation    -- :: Equation -> Equation
    , solve                 -- :: Equation -> Solution
    , solveLinear           -- :: Equation -> Solution
    , solveSimpleHyperbolic -- :: Equation -> Solution
    , solveEliptical        -- :: Equation -> Solution
    , solveParabolic        -- :: Equation -> Solution
    , toMaybeList           -- :: Solution -> Maybe [(Integer,Integer)]
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
-- > ax^2 + bxy + cy^2 + dx + ey + f = 0
data Equation = GeneralEquation Z Z Z Z Z Z      -- ^ A general quadratic
                                                 -- diophantine equation.
              | LinearEquation Z Z Z             -- ^ dx + ey + f = 0
              | SimpleHyperbolicEquation Z Z Z Z -- ^ bxy + dx +ey + f = 0
              | ElipticalEquation Z Z Z Z Z Z    -- ^ Eliptical equations.
              | ParabolicEquation Z Z Z Z Z Z    -- ^ Parabolic equations.
              | HyperbolicEquation Z Z Z Z Z Z   -- ^ Hyperbolic equations.
                deriving Show

-- | The results of attempting to solve an 'Equation'.
data Solution = ZxZ                 -- ^ All Integer pairs satisfy the equation.
              | NoSolutions         -- ^ For all (x,y) in ZxZ
              | SolutionSet [(Z,Z)] -- ^ The set of pairs (x,y) that satisfy the
                                    -- equation.
                deriving Eq

instance Show Solution where
    show  ZxZ             = "All Integers"
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
solve e = case specializeEquation e of
              e@(LinearEquation{})           -> solveLinear           e
              e@(SimpleHyperbolicEquation{}) -> solveSimpleHyperbolic e
              e@(ElipticalEquation{})        -> solveEliptical        e
              e@(ParabolicEquation{})        -> solveParabolic        e
              e@(HyperbolicEquation{})       -> error "not yet implemented"

-- | Detirmines what kind of equation form a GeneralEquation fits.
-- If you pass a non GeneralEquation to this function, it is the same as id.
specializeEquation :: Equation -> Equation
specializeEquation (GeneralEquation a b c d e f)
    | a == b && b == c && c == 0 = LinearEquation d e f
    | a == c && c == 0 && b /= 0 = SimpleHyperbolicEquation b d e f
    | b^2 - 4 * a * c < 0        = ElipticalEquation a b c d e f
    | b^2 - 4 * a * c == 0       = ParabolicEquation a b c d e f
    | b^2 - 4 * a * c > 0        = HyperbolicEquation a b c d e f
    | otherwise                  = error "Invalid equation type"
specializeEquation e = e

-- | Extracts the list of solution pairs from a solution.
toMaybeList :: Solution -> Maybe [(Z,Z)]
toMaybeList (SolutionSet ns) = Just ns
toMaybeList _                = Nothing

-- -------------------------------------------------------------------------- --
-- helper functions.

-- | Solves for the Bézout coefficients of a and b.
extendedGCD :: Integral a => a -> a -> (a,a)
extendedGCD a b = extendedGCD' 0 1 b 1 0 a
  where
      extendedGCD' _ _ 0 s' t' _  = (s',t')
      extendedGCD' s t r s' t' r' =
          let q = r' `div` r
          in extendedGCD' (s' - q * s) (t' - q * t) (r' - q * r)  s t r

-- | Returns a list of the divisors of n.
divisors :: Integral a => a -> [a]
divisors n =
    n:1:(concat [[x,n `div` x] | x <- [2..floor $ sqrt $ fromIntegral n]
                , n `rem` x == 0]
         \\ [floor $ sqrt $ fromIntegral n | isSquare n])
  where
      isSquare n = round (sqrt (fromIntegral n)) ^2 == n

-- -------------------------------------------------------------------------- --
-- exported solving functions.

-- | Solves for Equations in the form of dx + ey + f = 0
solveLinear :: Equation -> Solution
solveLinear (LinearEquation d e f)
    | d == 0 && e == 0 = let g     = gcd d e
                             (u,v) = extendedGCD d e
                         in if f == 0
                              then ZxZ
                              else solve' d e f g u v
    | d == 0 && e /= 0 = let g     = gcd d e
                             (u,v) = extendedGCD d e
                         in if f `mod` e == 0
                              then solve' d e f g u v
                              else NoSolutions
    | d /= 0 && e /= 0 = let g     = gcd d e
                             (u,v) = extendedGCD d e
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
solveLinear e =
    case specializeEquation e of
        e'@(LinearEquation{}) -> solveLinear e'
        _ -> error "solveLinear requires a linear equation"

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
solveSimpleHyperbolic e =
    case specializeEquation e of
        e'@(SimpleHyperbolicEquation{}) -> solveSimpleHyperbolic e'
        _ -> error "solveSimpleHyperbolic requires a simple hyperbolic equation"

-- | Solves for Equations in the form of ax^2 + bxy + cy^2 + dx + ey + f = 0
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
solveEliptical e =
    case specializeEquation e of
        e'@(ElipticalEquation{}) -> solveEliptical e'
        _ -> error "solveEliptical requires an eliptical equation"

-- | Solves for Equations in the form of ax^2 + bxy + cy^2  + dx + ey + f = 0
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
solveParabolic e =
    case specializeEquation e of
        e'@(ParabolicEquation{}) -> solveParabolic e'
        _ -> error "solveParabolic requires a parabolic equation."
