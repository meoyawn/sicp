module HigherProcedures where

import Prelude

simpsonsRule :: (Number -> Number) -> Number -> Number -> Number
simpsonsRule f a b = 
  where n = 100.0
        h = (b - a) / n
        y k = f (a + k * h)
