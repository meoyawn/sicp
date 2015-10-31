module Exponentiation where

import Prelude
import Basic

expt :: Number -> Int -> Number
expt = go 1.0
  where go product _ 0 = product
        go product b n = go (b * product) b (n - 1)

even :: Int -> Boolean
even n = n `mod` 2 == 0

fastExpt :: Number -> Int -> Number
fastExpt b 0 = 1.0
fastExpt b n
  | even n = square $ fastExpt b (n / 2)
  | otherwise = b * fastExpt b (n - 1)

-- 1.16
fastExptIter :: Number -> Int -> Number
fastExptIter = go 1.0
  where go product _ 0 = product
        go product b n
          | even n = go product (square b) (n / 2)
          | otherwise = go (b * product) b (n - 1)

mult :: forall a. (Ring a, Eq a) => a -> a -> a
mult a b
  | a == zero || b == zero = zero
  | otherwise = a + (mult a (b - one))

two :: forall a. (Semiring a) => a
two = one + one

double :: forall a. (Semiring a) => a -> a
double x = x * two

halve :: forall a. (ModuloSemiring a) => a -> a
halve x = x / two

-- 1.17
fastMult :: Int -> Int -> Int
fastMult 0 _ = 0
fastMult _ 0 = 0
fastMult 1 x = x
fastMult x 1 = x
fastMult a b
  | a < 0 = -fastMult (-a) b
  | b < 0 = -fastMult a (-b)
  | even a = double (fastMult (halve a) b)
  | even b = double (fastMult a (halve b))
  | otherwise = a + (fastMult a (b - one))
