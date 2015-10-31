module Exponentiation where

import Prelude

expt :: Number -> Int -> Number
expt = go 1.0
  where go product _ 0 = product
        go product b counter = go (b * product) b (counter - 1)
