module Basic where

import Prelude
import Data.Array
import Data.Foldable
import Debug.Trace

square :: forall a. (Semiring a) => a -> a
square x = x * x

sumSquares :: forall a f. (Functor f, Foldable f, Semiring a) => f a -> a
sumSquares = sum <<< map square

sumSquaresTwoLarger :: forall a. (Ord a, Semiring a) => a -> a -> a -> a
sumSquaresTwoLarger a b c = sumSquares <<< drop 1 <<< sort $ [a, b, c]

-- 1.10
ackermann :: Int -> Int -> Int
ackermann _ 0 = 0
ackermann _ 1 = 2
ackermann 0 y = 2 * y
ackermann x y = ackermann (x - 1) $ ackermann x (y - 1)

-- 2x
-- f = ackermann 0

-- 2^x
-- g = ackermann 1

-- x times 2^2
-- h = ackermann 2

-- 1.11
pascal :: Int -> Int
pascal n
  | n < 3     = n
  | otherwise = pascal (n - 1) + 2 * pascal (n - 2) + 3 * pascal (n - 3)

pascalIter :: Int -> Int -> Int -> Int -> Int
pascalIter _ _ x 0 = x
pascalIter newest middle oldest count = pascalIter (newest + 2 * middle + 3 * oldest) newest middle (count - 1)

pascalLin :: Int -> Int
pascalLin = pascalIter 2 1 0
