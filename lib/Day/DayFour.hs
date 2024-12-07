{-# LANGUAGE LambdaCase #-}

module Day.DayFour (printDayFourOne, printDayFourTwo) where

import Data.Foldable (Foldable (foldl'))
import Data.Functor
import Data.List (transpose)
import Data.List.Split (divvy)
import Data.Set qualified as Set
import Data.Void
import Text.Gigaparsec hiding (($>))
import Text.Gigaparsec.Char
import Text.Gigaparsec.Combinator
import Text.Gigaparsec.Debug (debug)
import Utils (diagonals, extract, parseAndPrint)

parseXmas :: Parsec String
parseXmas = string "XMAS" <|> string "SAMX"

runParseXmas :: String -> Result String Int
runParseXmas = parse @String (parseXmas $> 1)

solveLine :: String -> Int
solveLine input =
  let sliding = divvy 4 1 input
   in sum (extract $ fmap runParseXmas sliding)

sumLine :: [String] -> Int
sumLine input = sum $ fmap solveLine input

solve :: [String] -> Int
solve input =
  sumLine input + sumLine (transpose input) + sumLine (diagonals 4 input)

printDayFourOne :: String -> IO ()
printDayFourOne input = print $ solve (lines input)

printDayFourTwo :: String -> IO ()
printDayFourTwo input = undefined
