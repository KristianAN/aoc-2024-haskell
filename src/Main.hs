{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Functor ((<&>))
import Data.Text as T
import Solutions (printDayOneOne, printDayOneTwo)

readDay :: Int -> IO T.Text
readDay n = do
  let day = "day" <> show n <> ".txt"
  readFile day <&> T.pack

main :: IO ()
main = do
  one <- readDay 1
  printDayOneTwo one
