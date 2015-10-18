module Sqrt where

import Basic

import Prelude
import Data.Int

-- 1.7
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

-- 1.8
improveCube :: Number -> Number -> Number
improveCube guess x = ((x / square guess) + 2.0 * guess) / 3.0

sqrt :: Number -> Number
sqrt x = go 2.0 1.0 x
  where go = rootIter improve

cbrt :: Number -> Number
cbrt x = go 2.0 1.0 x
  where go = rootIter improveCube
