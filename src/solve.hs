-- Joe Jevnik
-- 20.2013
-- Diophantine equation solver.

import Control.Arrow ((***))
import Data.List ((\\),nub)
import Data.Ratio

-- -------------------------------------------------------------------------- --
-- data types.

type Z = Integer

-- A way to setup an equation in the form of ax^2 + by^2 + cxy + dx + ey + f = 0
data Equation = Equation Z Z Z Z Z Z
              | LinearEquation Z Z Z
              | SimpleHyperbolicEquation Z Z Z Z
              | ElipticalEquation Z Z Z Z Z Z
              | ParabolicEquation Z Z Z Z Z Z

-- The result data, All_Z meaning that all pairs of integers work, NoSolutions
-- means that no element of ZxZ satisfies the equation, and a SolutionSet is an
-- infinite list in the form of (x_n,y_n) where x and y satisfy the equation.
-- A solution set is in no particular order.
data Solution = All_Z | NoSolutions | SolutionSet [(Z,Z)]

instance Show Solution where
    show All_Z            = "All_Z"
    show NoSolutions      = "NoSolutions"
    show (SolutionSet ns) = show ns

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
solution_tolist :: Solution -> [(Z,Z)]
solution_tolist All_Z            = (0,0):[ (m,p) | n <- [1..]
                                         , m <- [n,-n], p <- [n,-n]]
solution_tolist NoSolutions      = []
solution_tolist (SolutionSet ns) = ns

-- -------------------------------------------------------------------------- --
-- solving functions.

solve :: Equation -> Solution
solve e@(LinearEquation{})           = solve_linear            e
solve e@(SimpleHyperbolicEquation{}) = solve_simple_hyperbolic e
solve e@(ElipticalEquation{})        = solve_eliptical         e
solve (Equation a b c d e f)
    | a == b && b == c && c == 0 = solve_linear
                                   (LinearEquation d e f)
    | a == c && c == 0 && b /= 0 = solve_simple_hyperbolic
                                   (SimpleHyperbolicEquation b d e f)
    | b^2 - 4 * a * c < 0        = solve_eliptical
                                   (ElipticalEquation a b c d e f)
    | b^2 - 4 * a * c == 0       = solve_parabolic
                                   (ParabolicEquation a b c d e f)
    | b^2 - 4 * a * c > 0        = error "Not yet implemented"
    | otherwise = error "Unknow Equation type"

-- Solves for Equations in the form of dx + ey + f = 0
solve_linear :: Equation -> Solution
solve_linear (LinearEquation d e f)
    | d == 0 && e == 0 = let g     = gcd d e
                             (u,v) = extended_gcd d e
                         in if f == 0
                              then All_Z
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
solve_linear _ = error "solve_linear requires a LinearEquation"

-- Solves for Equations in the form of bxy + dx + ey + f = 0
solve_simple_hyperbolic :: Equation -> Solution
solve_simple_hyperbolic (SimpleHyperbolicEquation b d e f)
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
solve_simple_hyperbolic _ =
    error "solve_simple_hyperbolic requires a SimpleHyperbolicEquation"

-- Solves for Equations in the form of ax^2 + bx^y + cy^2 + dx + ey + f = 0
-- when b^2 - 4ac < 0
solve_eliptical :: Equation -> Solution
solve_eliptical (ElipticalEquation a b c d e f) =
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
solve_eliptical _ = error "solve_eliptical requires ElipticalEquation"

-- Solves for Equations in the form of ax^2 + bx^y + cy^2 + dx + ey + f = 0
-- when b^2 - 4ac = 0
solve_parabolic :: Equation -> Solution
solve_parabolic (ParabolicEquation a b c d e f) =
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
                     (solution_tolist
                      (solve_linear (LinearEquation ra rc (-u_1))))
                     (solution_tolist
                      (solve_linear (LinearEquation ra rc (-u_2))))
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
