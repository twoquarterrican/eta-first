module ListGen where

import System.Random
import System.Random.Shuffle
import Data.List

distinct :: (RandomGen g) => g -> Int -> Int -> [Int]
distinct g uBound n
  | n <= 0     = []
  | n > uBound = []
  | n > midpt  = mixup g (all `complement` (distinct g uBound n'))
  | otherwise  = take n $ nub $ map (`mod` uBound) $ randoms g
  where midpt = uBound `div` 2
        all   = [0..uBound - 1]
        n'    = uBound - n

mixup :: (RandomGen g) => g -> [a] -> [a]
mixup g list = shuffle' list (length list) g

complement :: (Eq a) => [a] -> [a] -> [a]
complement includes excludes = filter (not . (`elem` excludes)) includes
