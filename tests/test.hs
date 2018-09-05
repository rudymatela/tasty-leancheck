-- Copyright (c) 2017-2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE CPP #-}
import Test.Tasty
import Test.Tasty.LeanCheck as LC

import Data.List (sort)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

-- loosely based on the official Tasty example
properties :: TestTree
properties = testGroup "(checked by LeanCheck)"
  [ LC.testProperty "sort . sort == sort" $
      \xs -> sort (sort xs :: [Int]) == sort xs
  , LC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
-- The following fails (and should):
--, LC.testProperty "Fermat's last theorem" $
--    \x y z n -> (n :: Integer) >= 3 ==> x^n + y^n /= (z^n :: Integer)
  ]
