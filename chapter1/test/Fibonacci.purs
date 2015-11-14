module Test.Fibonacci where

import Prelude
import Fibonacci
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Debug.Trace

newtype SmallNat = SmallNat Int
instance arbitrarySmallNat :: Arbitrary SmallNat where
  arbitrary = SmallNat <$> chooseInt 0 95

prop_Fib :: SmallNat -> Boolean
prop_Fib (SmallNat n) = (fibSlow n) == (fib n)
