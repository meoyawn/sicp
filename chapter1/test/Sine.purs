module Test.Sine where

import Prelude
import Sine
import Math

import Test.QuickCheck.Data.ApproxNumber

prop_Sine :: Number -> Boolean
prop_Sine x = sin x =~= sine x
