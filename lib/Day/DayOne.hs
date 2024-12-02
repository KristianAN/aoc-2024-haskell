module Day.DayOne (printDayOneOne, printDayOneTwo) where

import Control.Applicative
import Data.Bifunctor (Bifunctor (bimap))
import Data.Functor
import Data.List (sort)
import Data.Map as Map
import Data.Text as T
import Data.Void (Void)
import Text.Gigaparsec (Parsec, Result (Failure, Success), eof, lookAhead, parse)
import Text.Gigaparsec.Char (digit, endOfLine, newline, space, whitespaces)
import Text.Gigaparsec.Combinator
import Text.Gigaparsec.Debug (debug)
import Text.Gigaparsec.Errors.DefaultErrorBuilder

parseLine :: Parsec (Int, Int)
parseLine =
  do
    first <- manyTill digit space <&> read
    void whitespaces
    last <- manyTill digit (void $ lookAhead endOfLine) <&> read
    return (first, last)

parseNewlines :: Parsec [(Int, Int)]
parseNewlines = sepEndBy parseLine endOfLine

textToInt :: String -> Int
textToInt = read

-- Day One One
sortBoth :: ([Int], [Int]) -> ([Int], [Int])
sortBoth (fst, scnd) = (sort fst, sort scnd)

compareLists :: ([Int], [Int]) -> [Int]
compareLists = uncurry $ Prelude.zipWith (\x y -> abs (x - y))

solveOne :: [(Int, Int)] -> Int
solveOne =
  sum . compareLists . sortBoth . unzip

dayOne :: String -> Result String Int
dayOne = parse @String (parseNewlines <&> solveOne)

printDayOneOne :: String -> IO ()
printDayOneOne input = case dayOne input of
  Success sum -> putStrLn $ show sum
  Failure err -> putStrLn err

-- Day One Two
groupMapWithOccurences :: [Int] -> Map.Map Int Int
groupMapWithOccurences xs = Map.fromListWith (+) [(x, 1) | x <- xs]

countOccurrences :: [Int] -> Map.Map Int Int -> Int
countOccurrences input ocMap =
  sum $
    fmap
      ( \num ->
          let occ = Map.findWithDefault 0 num ocMap
           in num * occ
      )
      input

occurenceSum :: ([Int], [Int]) -> Int
occurenceSum (fst, second) =
  countOccurrences fst (groupMapWithOccurences second)

dayOneTwo :: String -> Result String Int
dayOneTwo = parse @String (parseNewlines <&> occurenceSum . unzip)

printDayOneTwo :: String -> IO ()
printDayOneTwo input = case dayOneTwo input of
  Success sum -> putStrLn $ show sum
  Failure err -> putStrLn err
