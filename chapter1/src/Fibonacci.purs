module Fibonacci where

import Prelude
import Basic
import Data.Int

fib :: Int -> Int
fib = go 1 0 0 1
  where go a b p q 0 = b
        go a b p q count
          | even count = go a b p q (count / 2)
          | otherwise = go newA newB p q (count - 1)
              where newA = b * q + a * q + a * p
                    newB = b * p + a * q
