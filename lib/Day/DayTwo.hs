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

parseLine :: Parsec [Int]
parseLine = manyTill (manyTill digit (space <|> lookAhead newline) <&> read) (lookAhead newline)

parseLines :: Parsec [[Int]]
parseLines = sepEndBy parseLine (void newline)

withinBounds :: Int -> Int -> Bool
withinBounds first second =
  diff > 0 && diff < 4
  where
    diff = abs (first - second)

data Elevation
  = Increasing
  | Decreasing
  | Same
  deriving (Eq, Show)

elevation :: Int -> Int -> Elevation
elevation first second
  | first > second = Decreasing
  | first < second = Increasing
  | otherwise = Same

accumulateScore :: Int -> Int -> [(Bool, Elevation)] -> [(Bool, Elevation)]
accumulateScore first second acc =
  let bounds = withinBounds first second
      elev = elevation first second
   in (bounds, elev) : acc

incOrDec :: [Elevation] -> Bool
incOrDec elev =
  all (== Increasing) elev || all (== Decreasing) elev

isValid :: [(Bool, Elevation)] -> Bool
isValid input =
  let (first, second) = unzip input
      oneWay = incOrDec second
   in oneWay && and first

safeLevel :: [Int] -> [(Bool, Elevation)]
safeLevel (first : second : tail) =
  let initial = accumulateScore first second []
   in fst $ foldl' (\(acc, prev) x -> (accumulateScore prev x acc, x)) (initial, second) tail

checkSafe :: [Int] -> Bool
checkSafe = isValid . safeLevel

safeDampenedLevel :: [Int] -> Bool
safeDampenedLevel input =
  checkSafe $
    foldl'
      ( \acc v ->
          if v < length acc
            then
              let (before, _ : after) = splitAt v acc
                  newList = before ++ after
               in if checkSafe newList then newList else acc
            else acc
      )
      input
      [0 .. length input - 1]

sumSafeLeves :: [[Int]] -> Int
sumSafeLeves input = length $ filter id (fmap (isValid . safeLevel) input)

sumSafeDampenedLevels :: [[Int]] -> Int
sumSafeDampenedLevels input =
  length $
    filter id $
      fmap
        ( \level ->
            checkSafe level || safeDampenedLevel level
        )
        input

printDayTwoOne :: String -> IO ()
printDayTwoOne input = case parse @String (parseLines <&> sumSafeLeves) input of
  Success sum -> print sum
  Failure err -> putStrLn err

printDayTwoTwo :: String -> IO ()
printDayTwoTwo input = case parse @String (parseLines <&> sumSafeDampenedLevels) input of
  Success sum -> print sum
  Failure err -> putStrLn err
