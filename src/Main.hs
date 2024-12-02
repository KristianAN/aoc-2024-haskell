{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Functor ((<&>))
import Data.Text as T
import Solutions (printDayOneOne, printDayOneTwo, printDayTwoOne, printDayTwoTwo)

readDay :: Int -> IO String
readDay n = do
  let day = "day" <> show n <> ".txt"
  readFile day

main :: IO ()
main = do
  one <- readDay 2
  printDayTwoTwo one
