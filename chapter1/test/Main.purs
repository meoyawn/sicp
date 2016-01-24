module Test.Main where

import Prelude
import Test.QuickCheck

import Test.Recursion
import Test.Exponentiation
import Test.Sqrt
import Test.Sine
import Test.Fibonacci
import Test.Divisors
import Test.HigherProcedures

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
  quickCheck prop_SmallestDivisor
  quickCheck prop_Primes
  quickCheck prop_Carmichael
  quickCheck prop_Integral
  quickCheck prop_SumIter
