module Divisors where

import Prelude
import Basic
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Date
import Data.Time

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

prime :: Int -> Boolean
prime n = n == smallestDivisor n

-- 1.22
timedPrimeTest :: forall e. Int -> Eff (console :: CONSOLE, now :: Now | e) Unit
timedPrimeTest n = do
  print n
  nowEpochMilliseconds >>= startPrimeTest n

startPrimeTest :: forall e. Int -> Milliseconds -> Eff (console :: CONSOLE, now :: Now | e) Unit
startPrimeTest n start = if prime n
                         then nowEpochMilliseconds >>= \now -> reportPrime (now - start)
                         else return unit

reportPrime :: forall e. Milliseconds -> Eff (console :: CONSOLE | e) Unit
reportPrime (Milliseconds ms) = do
  log $ " *** " ++ show ms
