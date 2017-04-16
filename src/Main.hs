module Main where

import ListGen
import System.Random
import System.IO

getInput :: String -> IO String
getInput msg = do
  putStr msg
  hFlush stdout
  getLine

main :: IO ()
main = do
  lim <- fmap read $ getInput "upper bound: "
  n   <- fmap read $ getInput "number of elements: "
  stdGen <- getStdGen
  let list = distinct stdGen lim n
  putStrLn $ "Here am " ++ show n ++ " random ints btwn 0 and "
       ++ show lim ++ ":\n"++ show list
