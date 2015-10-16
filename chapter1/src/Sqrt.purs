module Sqrt where

import Prelude
import Data.Int
import Basic

abs :: forall a. (Ord a, Ring a) => a -> a
abs x = if x < zero then -x else x

-- TODO small and large numbers
goodEnough :: Number -> Int -> Boolean
goodEnough guess x = (abs ((square guess) - toNumber x)) < 0.001

average :: Number -> Number -> Number
average x y = (x + y) / 2.0

improve :: Number -> Int -> Number
improve guess x = average guess (toNumber x / guess)

sqrtIter :: Number -> Int -> Number
sqrtIter guess x = if goodEnough guess x then guess
                   else sqrtIter (improve guess x) x
