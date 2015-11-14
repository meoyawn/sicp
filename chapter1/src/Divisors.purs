module Divisors where

import Prelude
import Basic

-- 1.21
smallestDivisor :: Int -> Int
smallestDivisor n = findDivisor n 2

findDivisor :: Int -> Int -> Int
findDivisor n test
  | square test > n = n
  | divides test n = test
  | otherwise = findDivisor n (test + 1)

divides :: Int -> Int -> Boolean
divides a b = b `mod` a == 0
