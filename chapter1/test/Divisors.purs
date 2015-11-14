module Test.Divisors where

import Prelude
import Divisors
import Basic

prop_SmallestDivisor :: Int -> Boolean
prop_SmallestDivisor n = (abs $ smallestDivisor n) <= abs n
