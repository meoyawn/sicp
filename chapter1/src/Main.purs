module Main where

import Basic
import Sqrt

import Prelude
import Control.Monad.Eff.Console

main = do
  print $ cbrt 216000000.0
