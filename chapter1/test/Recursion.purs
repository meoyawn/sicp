module Test.Recursion where

import Prelude
import Recursion

prop_F :: Int -> Boolean
prop_F x = 2 * x == f x
