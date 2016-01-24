module HigherProcedures where

import Prelude
import Data.Int

sum :: forall t n. (Ord t, Semiring n) => (t -> n) -> t -> (t -> t) -> t -> n
sum term a next b
  | a > b = zero
  | otherwise = (term a) + (sum term (next a) next b)

-- 1.30
sumIter :: forall t n. (Ord t, Semiring n) => (t -> n) -> t -> (t -> t) -> t -> n
sumIter term a next b = go a zero
  where go a result
         | a > b     = result
         | otherwise = go (next a) (result + term a)

inc :: forall a. (Semiring a) => a -> a
inc n = n + one

integral :: (Number -> Number) -> Number -> Number -> Number -> Number
integral f a b dx = (sum f (a + dx / 2.0) addDx b) * dx
  where addDx x = x + dx

-- 1.29
simpsonsRule :: (Number -> Number) -> Number -> Number -> Number
simpsonsRule f a b = h / 3.0 * (sum func 0 inc n)
  where n = 100
        h = (b - a) / (toNumber n)
        y k = f (a + (toNumber k) * h)
        func k
          | k == 0 || k == n = y k
          | k `mod` 2 == 0   = 2.0 * (y k)
          | otherwise        = 4.0 * (y k)
