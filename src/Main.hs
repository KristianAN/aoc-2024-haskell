module Main where

import Solutions (printDayOneOne, printDayOneTwo, printDayThreeOne, printDayTwoOne, printDayTwoTwo)

readDay :: Int -> IO String
readDay n = do
  let day = "day" <> show n <> ".txt"
  readFile day

main :: IO ()
main = do
  one <- readDay 2
  printDayThreeOne
