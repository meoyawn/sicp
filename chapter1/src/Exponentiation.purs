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

fastExptIter :: Number -> Int -> Number
fastExptIter = go 1.0
  where go product _ 0 = product
        go product b n
          | even n = go product (square b) (n / 2)
          | otherwise = go (b * product) b (n - 1)
