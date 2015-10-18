module Sqrt where

import Basic

import Prelude
import Data.Int

abs :: forall a. (Ord a, Ring a) => a -> a
abs x = if x < zero then -x else x

goodEnough :: Number -> Number -> Number -> Boolean
goodEnough prevGuess guess x = distance prevGuess guess < 0.000000000000001

average :: Number -> Number -> Number
average x y = (x + y) / 2.0

distance :: Number -> Number -> Number
distance x y = abs (x - y)

rootIter :: (Number -> Number -> Number) -> Number -> Number -> Number  -> Number
rootIter imp prevGuess guess x = if goodEnough prevGuess guess x then guess
                                 else rootIter imp guess (imp guess x) x

improve :: Number -> Number -> Number
improve guess x = average guess (x / guess)

improveCube :: Number -> Number -> Number
improveCube guess x = ((x / square guess) + 2.0 * guess) / 3.0

sqrt :: Number -> Number
sqrt x = go 2.0 1.0 x
  where go = rootIter improve

cbrt :: Number -> Number
cbrt x = go 2.0 1.0 x
  where go = rootIter improveCube
