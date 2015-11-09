module Test.Sqrt where

import Prelude
import Basic
import Sqrt

import Test.QuickCheck.Data.ApproxNumber

prop_Sqrt :: Number -> Boolean
prop_Sqrt x = sqrt (square x) =~= x

prop_Cbrt :: Number -> Boolean
prop_Cbrt x = cbrt (cube x) =~= x
