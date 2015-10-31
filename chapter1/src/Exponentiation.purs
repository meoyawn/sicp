module Exponentiation where

import Prelude
import Basic

expt :: Number -> Int -> Number
expt = go 1.0
  where go product _ 0 = product
        go product b counter = go (b * product) b (counter - 1)

even :: Int -> Boolean
even n = n `mod` 2 == 0

fastExpt :: Number -> Int -> Number
fastExpt b 0 = 1.0
fastExpt b n
  | even n = square $ fastExpt b (n / 2)
  | otherwise = b * fastExpt b (n - 1)
