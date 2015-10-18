module Main where

import Basic
import Sqrt

import Prelude
import Control.Monad.Eff.Console

main = do
  print $ sqrt 99999999999999.0
