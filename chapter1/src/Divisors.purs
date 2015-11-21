module Divisors where

import Prelude
import Basic

import Data.Date
import Data.Time
import Data.Int (even)

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Random

-- 1.21
smallestDivisor :: Int -> Int
smallestDivisor n = findDivisor n 2

findDivisor :: Int -> Int -> Int
findDivisor n test
  | square test > n = n
  | divides test n = test
  | otherwise = findDivisor n $ next test

-- 1.23
next :: Int -> Int
next 2 = 3
next n = n + 2

divides :: Int -> Int -> Boolean
divides a b = b `mod` a == 0

prime :: Int -> Boolean
prime n = n == smallestDivisor n

-- 1.22
timedPrimeTest :: forall e. Int -> Eff (console :: CONSOLE, now :: Now | e) Boolean
timedPrimeTest n = do
  print n
  nowEpochMilliseconds >>= startPrimeTest n

startPrimeTest :: forall e. Int -> Milliseconds -> Eff (console :: CONSOLE, now :: Now | e) Boolean
startPrimeTest n start = if prime n
                         then do
                           now <- nowEpochMilliseconds
                           reportPrime (now - start)
                           return true
                         else return false

reportPrime :: forall e. Milliseconds -> Eff (console :: CONSOLE | e) Unit
reportPrime (Milliseconds ms) = do
  log $ " *** " ++ show ms

searchForPrimes :: forall e. Int -> Int -> Eff (console :: CONSOLE, now :: Now | e) Unit
searchForPrimes _ 0 = return unit
searchForPrimes n count = do
  p <- timedPrimeTest n
  if p then searchForPrimes nextN (count - 1) else searchForPrimes nextN count
    where nextN = n + 1

-- 1.24
expMod :: Int -> Int -> Int -> Int
expMod _ 0 _ = 1
expMod base exp m
  | even exp = (square $ expMod base (exp / 2) m) `mod` m
  | otherwise = (base * expMod base (exp - 1) m) `mod` m

fermatTest :: forall e. Int -> Eff (random :: RANDOM | e) Boolean
fermatTest n = do
  r <- randomInt 0 (n - 1)
  return $ tryIt (1 + r)
    where tryIt a = (expMod a n n) == a

fastPrime :: forall e. Int -> Int -> Eff (random :: RANDOM | e) Boolean
fastPrime _ 0 = return true
fastPrime n times = do
  f <- fermatTest n
  if f then fastPrime n (times - 1) else return false
