module Test.Exponentiation where

import Prelude
import Math
import Data.Int (toNumber)

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Data.ApproxNumber

import Exponentiation

newtype Power = Power Int
instance arbitraryPower :: Arbitrary Power where
  arbitrary = Power <$> chooseInt 0 100000

prop_Expt :: Number -> Power -> Boolean
prop_Expt b (Power n) = pow b (toNumber n) =~= expt b n
