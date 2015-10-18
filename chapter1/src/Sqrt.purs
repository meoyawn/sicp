module Sqrt where

import Basic

import Prelude
import Data.Int

abs :: forall a. (Ord a, Ring a) => a -> a
abs x = if x < zero then -x else x

-- TODO small and large numbers
goodEnough :: Number -> Number -> Number -> Boolean
goodEnough prevGuess guess x = distance prevGuess guess < 0.000000000000001

average :: Number -> Number -> Number
average x y = (x + y) / 2.0

improve :: Number -> Number -> Number
improve guess x = average guess (x / guess)

distance :: Number -> Number -> Number
distance x y = abs (x - y)

sqrtIter :: Number -> Number -> Number -> Number
sqrtIter prevGuess guess x = if goodEnough prevGuess guess x then guess
                             else sqrtIter guess (improve guess x) x

sqrt :: Number -> Number
sqrt x = sqrtIter 2.0 1.0 x
