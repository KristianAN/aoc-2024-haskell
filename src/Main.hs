module Main where

import Solutions (printDayFourOne, printDayOneOne, printDayOneTwo, printDayThreeOne, printDayTwoOne, printDayTwoTwo)

readDay :: Int -> IO String
readDay n = do
  let day = "day" <> show n <> ".txt"
  readFile day

main :: IO ()
main = do
  one <- readDay 4
  printDayFourOne one
