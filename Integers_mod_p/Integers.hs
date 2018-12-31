module Integers where

import Control.Exception

extGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extGCD a 0 = (1, 0, a)
extGCD a b = (t, s - q * t, abs g)
            where
                (q, r) = a `quotRem` b
                (s, t, g) = extGCD b r

-- IntModP n p - n is an integer modulo p
data IntModP = IntModP Integer Integer deriving (Show, Eq)

-- addition
addModP :: IntModP -> IntModP -> IntModP
addModP x y = IntModP (assert (xp == yp) ((xn + yn) `mod` xp)) xp
            where
                IntModP xn xp = x
                IntModP yn yp = y

-- subtraction
subModP :: IntModP -> IntModP -> IntModP
subModP x y = IntModP (assert (xp == yp) ((xn - yn) `mod` xp)) xp
            where
                IntModP xn xp = x
                IntModP yn yp = y

-- additive inverse
addInvModP :: IntModP -> IntModP
addInvModP (IntModP n p) = IntModP (p - n `mod` p) p

-- multiplication
mulModP :: IntModP -> IntModP -> IntModP
mulModP x y = IntModP (assert (xp == yp) ((xn * yn) `mod` xp)) xp
            where
                IntModP xn xp = x
                IntModP yn yp = y

-- multiplicative inverse
-- p must be prime
mulInvModP :: IntModP -> IntModP
mulInvModP (IntModP n p) = IntModP (i `mod` p) p
                        where
                            -- get first element of the extGCD tuple
                            -- that represents
                            -- i * n + _ * p = gcd(n, p) = 1 (since p is prime)
                            (i, _, _) = extGCD n p

-- division
-- p must be prime
divModP :: IntModP -> IntModP -> IntModP
divModP x y = IntModP (assert (xp == yp) ((xn * yn) `mod` xp)) xp
            where
                IntModP xn xp = x
                IntModP yn yp = mulInvModP y

-- exponentiation
-- p must be prime
-- http://www.cs.ucf.edu/~dmarino/progcontests/modules/matexpo/RecursionFastExp.pdf
-- expModP b e = b^e mod p
expModP :: IntModP -> Integer -> IntModP
expModP b e
          | e == 0 = IntModP 1 bp
          | e == 1 = b
          | e `mod` 2 == 0 = expModP (b `mulModP` b) (e `div` 2)
          | otherwise = b `mulModP` (expModP b (e - 1))
            where
                IntModP bn bp = b