module Test.Main where

import Prelude
import Control.Monad.Eff.Console
import Test.QuickCheck
import Test.Recursion

main :: forall eff. QC eff Unit
main = do
  quickCheck prop_F
