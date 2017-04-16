module Main where

import ListGen
import Test.QuickCheck
import System.Random
import Data.List

main = quickCheck prop_size

prop_size (Positive n) seed = n == length (distinct (mkStdGen seed) (n+1) n)
prop_distinct uBound n seed = nub generated == generated where
  generated = distinct (mkStdGen seed) uBound n

