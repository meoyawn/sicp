module Basic where

import Prelude
import Data.Array
import Data.Foldable

square :: forall a. (Semiring a) => a -> a
square x = x * x

sumSquares :: forall a f. (Functor f, Foldable f, Semiring a) => f a -> a
sumSquares = sum <<< map square

sumSquaresTwoLarger :: forall a. (Ord a, Semiring a) => a -> a -> a -> a
sumSquaresTwoLarger a b c = sumSquares <<< drop 1 <<< sort $ [a, b, c]
