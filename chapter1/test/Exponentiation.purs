module Test.Exponentiation where

import Prelude ((==), (*), negate, (<$>))
import Math (pow)
import Data.Int (toNumber)

import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt)
import Test.QuickCheck.Data.ApproxNumber ((=~=))

import Exponentiation (russianPeasant, fastMult, fastExptIter, fastExpt, expt)

newtype SmallPower = SmallPower Int
instance arbitrarySmallPower :: Arbitrary SmallPower where
  arbitrary = SmallPower <$> chooseInt 0 99999

prop_Expt :: Number -> SmallPower -> Boolean
prop_Expt b (SmallPower n) = pow b (toNumber n) =~= expt b n

newtype BigPower = BigPower Int
instance arbitraryBigPower :: Arbitrary BigPower where
  arbitrary = BigPower <$> chooseInt 0 2147483647

prop_FastExpt :: Number -> BigPower -> Boolean
prop_FastExpt b (BigPower n) = pow b (toNumber n) =~= fastExpt b n

prop_FastExptIter :: Number -> BigPower -> Boolean
prop_FastExptIter b (BigPower n) = pow b (toNumber n) =~= fastExptIter b n

newtype SmallInt = SmallInt Int
instance arbitrarySmallInt :: Arbitrary SmallInt where
  arbitrary = SmallInt <$> chooseInt (-500) 500

prop_Mult :: Int -> Int -> Boolean
prop_Mult a b = a * b == fastMult a b

-- those are smaller ints because I don't want to fuck with bigints just yet
prop_RussianPeasant :: SmallInt -> SmallInt -> Boolean
prop_RussianPeasant (SmallInt a) (SmallInt b) = a * b == russianPeasant a b
