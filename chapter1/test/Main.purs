module Test.Main where

import Prelude (Unit, bind)
import Test.QuickCheck (QC, quickCheck)

import Test.Recursion (prop_F)
import Test.Exponentiation (prop_RussianPeasant, prop_Mult, prop_FastExptIter, prop_FastExpt, prop_Expt)
import Test.Sqrt (prop_Cbrt, prop_Sqrt)
import Test.Sine (prop_Sine)
import Test.Fibonacci (prop_Fib)
import Test.Divisors (prop_Carmichael, prop_Primes, prop_SmallestDivisor)
import Test.HigherProcedures (prop_Product, prop_SumIter, prop_Integral)

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
  quickCheck prop_Product
