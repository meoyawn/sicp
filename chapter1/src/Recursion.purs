module Recursion where

import Prelude

-- 1.10
ackermann :: Int -> Int -> Int
ackermann _ 0 = 0
ackermann _ 1 = 2
ackermann 0 y = 2 * y
ackermann x y = ackermann (x - 1) $ ackermann x (y - 1)

-- 2x
f = ackermann 0

-- 2^x
g = ackermann 1

-- x times 2^2
h = ackermann 2

-- 1.11
fRec :: Int -> Int
fRec n
  | n < 3     = n
  | otherwise = fRec (n - 1) + 2 * fRec (n - 2) + 3 * fRec (n - 3)

fIter :: Int -> Int
fIter = go 2 1 0
  where go _ _ x 0 = x
        go newest middle oldest count = go (newest + 2 * middle + 3 * oldest) newest middle (count - 1)

-- 1.12
pascal :: Int -> Int -> Int
pascal 0 0 = 1
pascal level pos
  | level < 0 || pos < 0 || pos > level = 0
  | otherwise = parent (pos - 1) + parent pos
      where parent = pascal (level - 1)
