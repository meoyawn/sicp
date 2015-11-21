module Test.Divisors where

import Prelude
import Divisors
import Basic

import Control.Monad.Eff
import Control.Monad.Eff.Unsafe
import Control.Monad.Eff.Random

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

import Debug.Trace

prop_SmallestDivisor :: Int -> Boolean
prop_SmallestDivisor n = (abs $ smallestDivisor n) <= abs n

newtype BoolEff e = BoolEff (Eff e Boolean)

instance testableBooleanEff :: Testable (BoolEff e) where
  test (BoolEff eff) = test $ unsafePerformEff eff

newtype Nat = Nat Int
instance arbitraryNat :: Arbitrary Nat where
  arbitrary = Nat <$> chooseInt 0 560

prop_Primes :: forall e. Nat -> BoolEff (random :: RANDOM | e)
prop_Primes (Nat n) = BoolEff $ do
  fp <- fastPrime n 10
  return $ fp == prime n
