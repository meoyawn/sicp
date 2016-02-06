module Test.Divisors where

import Prelude (return, ($), (==), bind, (<$>), (<=))
import Divisors (fastPrime, congruent, prime, smallestDivisor)
import Basic (abs)
import Test.Exponentiation (BigPower(BigPower))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Random (RANDOM)

import Test.QuickCheck (class Testable, test)
import Test.QuickCheck.Gen (chooseInt)
import Test.QuickCheck.Arbitrary (class Arbitrary)

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

prop_Carmichael :: forall e. BigPower -> BoolEff (random :: RANDOM | e)
prop_Carmichael (BigPower n) = BoolEff $ do
  if congruent n then fastPrime n 10 else return true
