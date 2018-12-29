module Integers where

import Control.Exception

-- IntModP n p - n is an integer modulo p
data IntModP = IntModP Integer Integer deriving (Show, Eq)

-- addition
addModP :: IntModP -> IntModP -> IntModP
addModP (IntModP xn xp) (IntModP yn yp) = IntModP (assert (xp == yp) ((xn + yn) `mod` xp)) xp

-- subtraction
subModP :: IntModP -> IntModP -> IntModP
subModP (IntModP xn xp) (IntModP yn yp) = IntModP (assert (xp == yp) ((xn - yn) `mod` xp)) xp

-- the additive inverse
addInvModP :: IntModP -> IntModP
addInvModP (IntModP n p) = IntModP (p - n `mod` p) p

-- multiplication
mulModP :: IntModP -> IntModP -> IntModP
mulModP (IntModP xn xp) (IntModP yn yp) = IntModP (assert (xp == yp) ((xn * yn) `mod` xp)) xp
