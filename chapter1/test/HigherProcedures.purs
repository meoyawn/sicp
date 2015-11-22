module Test.HigherProcedures where

import Prelude
import Basic
import HigherProcedures

import Data.Tuple
import Debug.Trace

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

-- Approximate equality comparison
(=~=) :: Number -> Number -> Boolean
(=~=) x y = (y - x) <= eps && (y - x) >= (-eps)
  where eps = 0.1

data LeftRight = LeftRight Number Number

instance arbitraryLeftRight :: Arbitrary LeftRight where
  arbitrary = do
    l <- choose (-2.0) 2.0
    r <- choose l 2.0
    return $ LeftRight l r

prop_Integral :: LeftRight -> Boolean
prop_Integral (LeftRight a b) = left =~= right
  where left  = integral cube a b 0.01
        right = simpsonsRule cube a b
