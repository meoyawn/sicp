module Test.Main where

import Prelude
import Control.Monad.Eff.Console
import Test.QuickCheck

import Test.Recursion
import Test.Exponentiation
import Test.Sqrt
import Test.Sine
import Test.Fibonacci

main :: forall eff. QC eff Unit
main = do
  quickCheck prop_F
  quickCheck prop_Expt
  quickCheck prop_FastExpt
  quickCheck prop_FastExptIter
  quickCheck prop_Mult
  quickCheck prop_RussianPeasant
  quickCheck prop_Sqrt
  quickCheck prop_Cbrt
  quickCheck prop_Sine
  quickCheck prop_Fib
