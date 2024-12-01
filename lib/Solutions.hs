module Solutions (printDayOneOne, printDayOneTwo) where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Functor
import Data.List (sort)
import Data.Map as Map
import Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline, space)
import Text.Megaparsec.Debug (MonadParsecDbg (dbg))

type Parser = Parsec Void T.Text

type ParserErrors = ParseErrorBundle T.Text Void

parseLine :: Parser (T.Text, T.Text)
parseLine =
  try $
    do
      first <- takeWhileP (Just "first number") (/= ' ')
      _ <- space
      last <- takeWhileP (Just "second number") (/= '\n')
      return (first, last)

parseNewlines :: Parser [(T.Text, T.Text)]
parseNewlines = sepBy parseLine newline

parseDay1 :: T.Text -> Either ParserErrors [(T.Text, T.Text)]
parseDay1 = runParser parseNewlines "aoc input day one"

textToInt :: Text -> Int
textToInt = read . unpack

toIntList :: [(T.Text, T.Text)] -> [(Int, Int)]
toIntList = fmap (bimap textToInt textToInt) . Prelude.init

-- Day One One
sortBoth :: ([Int], [Int]) -> ([Int], [Int])
sortBoth (fst, scnd) = (sort fst, sort scnd)

compareLists :: ([Int], [Int]) -> [Int]
compareLists = uncurry $ Prelude.zipWith (\x y -> abs (x - y))

solveOne :: [(Int, Int)] -> Int
solveOne =
  sum . compareLists . sortBoth . unzip

dayOne :: T.Text -> Either ParserErrors Int
dayOne input = parseDay1 input <&> solveOne . toIntList

printDayOneOne :: T.Text -> IO ()
printDayOneOne input = case dayOne input of
  Right sum -> putStrLn $ show sum
  Left err -> putStrLn $ errorBundlePretty err

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

dayOneTwo :: T.Text -> Either ParserErrors Int
dayOneTwo input = parseDay1 input <&> occurenceSum . unzip . toIntList

printDayOneTwo :: T.Text -> IO ()
printDayOneTwo input = case dayOneTwo input of
  Right sum -> putStrLn $ show sum
  Left err -> putStrLn $ errorBundlePretty err
