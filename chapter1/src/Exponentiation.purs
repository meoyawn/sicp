module Exponentiation where

import Prelude
import Basic
import Debug.Trace
import Data.Tuple

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

-- 1.16
fastExptIter :: Number -> Int -> Number
fastExptIter = go 1.0
  where go product _ 0 = product
        go product b n
          | even n = go product (square b) (n / 2)
          | otherwise = go (b * product) b (n - 1)

-- 1.17
fastMult :: Int -> Int -> Int
fastMult _ 0 = 0
fastMult a b
  | a < 0 = -fastMult (-a) b
  | b < 0 = -fastMult a (-b)
  | even b = double (fastMult a (halve b))
  | otherwise = a + (fastMult a (b - one))

-- 1.18
russianPeasant :: Int -> Int -> Int
russianPeasant = go zero
  where go sum a b
          | a < zero = traceShow (Tuple a b) \_ -> go sum (-a) b
          | b < zero = traceShow (Tuple a b) \_ -> go sum a (-b)
          | b == one = traceShow (Tuple a b) \_ -> sum + a
          | even b = traceShow (Tuple a b) \_ -> go sum (double a) (halve b)
          | otherwise = traceShow (Tuple a b) \_ -> go (sum + a) (double a) (halve b)
