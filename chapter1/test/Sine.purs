module Test.Sine where

import Sine (sine)
import Math (sin)
import Test.QuickCheck.Data.ApproxNumber ((=~=))

prop_Sine :: Number -> Boolean
prop_Sine x = sin x =~= sine x
