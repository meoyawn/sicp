module Basic where

import Prelude
import Data.Array

square :: forall a. (Ring a) => a -> a
square x = x * x

sumSquaresTwo :: Array Int -> Int
sumSquaresTwo [x, y] = (square x) + (square y)
sumSquaresTwo _ = -1

sumSquaresTwoLarger :: Int -> Int -> Int -> Int
sumSquaresTwoLarger a b c = sumSquaresTwo <<< drop 1 <<< sort $ [a, b, c]
