module Fibonacci where

import Prelude
import Basic
import Data.Int

fibSlow :: Int -> Int
fibSlow = go 0 1
  where go _ _ 0 = 0
        go _ _ 1 = 1
        go earlier later 2 = earlier + later
        go earlier later count = go later (earlier + later) (count - 1)

-- 1.19
fib :: Int -> Int
fib = go 1 0 0 1
  where go a b p q 0 = b
        go a b p q count
          | even count = go a b newP newQ (count / 2)
              where newP = square p + qSquared
                    newQ = 2 * p * q + qSquared
                    qSquared = square q
          | otherwise = go newA newB p q (count - 1)
              where newA = b * q + a * q + a * p
                    newB = b * p + a * q
