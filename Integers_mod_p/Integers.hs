module Integers where

import Control.Exception

-- IntModP n p - n is an integer modulo p
data IntModP = IntModP Integer Integer deriving (Show, Eq)

addModP :: IntModP -> IntModP -> IntModP
addModP (IntModP xn xp) (IntModP yn yp) = IntModP (assert (xp == yp) ((xn + yn) `mod` xp)) xp

subModP :: IntModP -> IntModP -> IntModP
subModP (IntModP xn xp) (IntModP yn yp) = IntModP (assert (xp == yp) ((xn - yn) `mod` xp)) xp