module Main where

import Prelude
import Data.Date
import Control.Monad.Eff
import Control.Monad.Eff.Console

import Divisors

main :: forall e. Eff (console :: CONSOLE, now :: Now | e) Unit
main = timedPrimeTest 19999999
