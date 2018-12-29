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
addModP (IntModP xn xp) (IntModP yn yp) = IntModP (assert (xp == yp) ((xn + yn) `mod` xp)) xp

-- subtraction
subModP :: IntModP -> IntModP -> IntModP
subModP (IntModP xn xp) (IntModP yn yp) = IntModP (assert (xp == yp) ((xn - yn) `mod` xp)) xp

-- additive inverse
addInvModP :: IntModP -> IntModP
addInvModP (IntModP n p) = IntModP (p - n `mod` p) p

-- multiplication
mulModP :: IntModP -> IntModP -> IntModP
mulModP (IntModP xn xp) (IntModP yn yp) = IntModP (assert (xp == yp) ((xn * yn) `mod` xp)) xp

-- multiplicative inverse
mulInvModP :: IntModP -> IntModP
mulInvModP (IntModP n p) = IntModP (i `mod` p) p
                        where
                            -- get first element of the extGCD tuple
                            -- that represents
                            -- i * n + _ * p = gcd(n, p) = 1 (since p is prime)
                            (i, _, _) = extGCD n p