module Test.HigherProcedures where

import Prelude
import Data.Generic
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

import Basic
import HigherProcedures

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

data LeftRightInt = LeftRightInt Int Int
derive instance genericLeftRightInt :: Generic LeftRightInt
instance showLeftRightInt :: Show LeftRightInt where
  show = gShow

instance arbitraryLeftRightInt :: Arbitrary LeftRightInt where
  arbitrary = do
    l <- chooseInt (-1000) 1000
    r <- chooseInt l 1000
    return $ LeftRightInt l r

prop_SumIter :: LeftRightInt -> Boolean
prop_SumIter (LeftRightInt a b) = s1 == s2
  where s1 = sum inc a inc b
        s2 = sumIter inc a inc b
