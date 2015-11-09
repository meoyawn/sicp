module Basic where

import Prelude
import Data.Array
import Data.Foldable
import Debug.Trace

square :: forall a. (Semiring a) => a -> a
square x = x * x

cube :: forall a. (Num a) => a -> a
cube x = x * x * x

abs :: forall a. (Ord a, Ring a) => a -> a
abs x = if x < zero then -x else x

sumSquares :: forall a f. (Functor f, Foldable f, Semiring a) => f a -> a
sumSquares = sum <<< map square

sumSquaresTwoLarger :: forall a. (Ord a, Semiring a) => a -> a -> a -> a
sumSquaresTwoLarger a b c = sumSquares <<< drop 1 <<< sort $ [a, b, c]
