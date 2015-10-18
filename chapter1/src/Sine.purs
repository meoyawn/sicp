module Sine where

import Basic

import Prelude

cube :: forall a. (Num a) => a -> a
cube x = x * x * x

three :: forall a. (Num a) => a
three = one + one + one

four :: forall a. (Num a) => a
four = three + one

p :: forall a. (Num a) => a -> a
p x = three * x - four * cube x

-- 1.15
sine :: Number -> Number
sine angle
  | (abs angle) <= 0.0001 = angle
  | otherwise             = p <<< sine $ angle / three
