module Day.DayTwo (printDayTwoOne, printDayTwoTwo) where

import Control.Applicative
import Data.Char (digitToInt)
import Data.Foldable (Foldable (foldl'), foldr')
import Data.Functor
import Data.Void (Void)
import Day.DayOne (printDayOneTwo)
import Text.Gigaparsec (Parsec, Result (..), eof, lookAhead, parse)
import Text.Gigaparsec.Char (digit, endOfLine, newline, space, whitespace)
import Text.Gigaparsec.Combinator (endBy, manyTill, option, sepBy, sepEndBy, sepEndBy1)
import Utils (parseAndPrint)

parseLine :: Parsec [Int]
parseLine = manyTill (manyTill digit (space <|> lookAhead newline) <&> read) (lookAhead newline)

parseLines :: Parsec [[Int]]
parseLines = sepEndBy parseLine (void newline)

data Elevation
  = Increasing
  | Decreasing
  | Same
  deriving (Eq, Show)

accumulateScore :: Int -> Int -> [(Bool, Elevation)] -> [(Bool, Elevation)]
accumulateScore first second acc =
  let bounds = diff > 0 && diff < 4 where diff = abs (first - second)
      elevation
        | first > second = Decreasing
        | first < second = Increasing
        | otherwise = Same
   in (bounds, elevation) : acc

isValid :: [(Bool, Elevation)] -> Bool
isValid input =
  let (first, second) = unzip input
      oneWay = (all (== Increasing) second || all (== Decreasing) second)
   in oneWay && and first

safeLevel :: [Int] -> [(Bool, Elevation)]
safeLevel (first : second : tail) =
  let initial = accumulateScore first second []
   in fst $ foldl' (\(acc, prev) x -> (accumulateScore prev x acc, x)) (initial, second) tail

safeDampenedLevel :: [Int] -> Bool
safeDampenedLevel input =
  (isValid . safeLevel) input
    || ( isValid . safeLevel $
           foldl'
             ( \acc v ->
                 if v < length acc
                   then
                     let (before, _ : after) = splitAt v acc
                         newList = before ++ after
                      in if (isValid . safeLevel) newList then newList else acc
                   else acc
             )
             input
             [0 .. length input - 1]
       )

sumSafeLeves :: [[Int]] -> Int
sumSafeLeves input = length $ filter id (fmap (isValid . safeLevel) input)

sumSafeDampenedLevels :: [[Int]] -> Int
sumSafeDampenedLevels input = length $ filter id $ fmap safeDampenedLevel input

printDayTwoOne :: String -> IO ()
printDayTwoOne = parseAndPrint $ parseLines <&> sumSafeLeves

printDayTwoTwo :: String -> IO ()
printDayTwoTwo = parseAndPrint $ parseLines <&> sumSafeDampenedLevels
