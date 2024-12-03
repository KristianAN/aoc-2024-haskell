{-# LANGUAGE LambdaCase #-}

module Day.DayThree (printDayThreeOne, printDayThreeTwo) where

import Data.Functor
import Data.List.Split (divvy)
import Data.Set qualified as Set
import Data.Void
import Text.Gigaparsec hiding (($>))
import Text.Gigaparsec.Char
import Text.Gigaparsec.Combinator
import Text.Gigaparsec.Debug (debug)
import Utils (parseAndPrint)

-- part one
parseNumber :: Parsec Int
parseNumber = manyTill digit (char ',' <|> char ')') <&> read

parseValidMul :: Parsec (Int, Int)
parseValidMul = string "mul(" *> parseNumber <~> parseNumber <* skipMany item <* eof

parseMul = parse @String parseValidMul

runParsers :: String -> [(Int, Int)]
runParsers input =
  let sliding = divvy 13 1 input
      parsed =
        fmap
          ( ( \case
                Success v -> v
                Failure _ -> (0, 0)
            )
              . parseMul
          )
          sliding
   in parsed

solve :: [(Int, Int)] -> Int
solve = sum . fmap (uncurry (*))

-- part two

data Instruction
  = Mul {nums :: (Int, Int)}
  | Enable
  | Disable
  deriving (Eq, Show)

parseDont = string "don't()" $> Disable <* skipMany item <* eof

parseDo = string "do()" $> Enable <* skipMany item <* eof

parseMul' = parseValidMul <&> Mul

p2parser :: Parsec Instruction
p2parser = atomic parseDo <|> parseDont <|> parseMul'

runParsers2 :: String -> [Instruction]
runParsers2 input =
  let sliding = divvy 13 1 input
      parsed =
        fmap
          ( ( \case
                Success v -> v
                Failure _ -> Mul (0, 0)
            )
              . parse @String p2parser
          )
          sliding
   in parsed

solve2 :: String -> Int
solve2 input =
  let instructions = runParsers2 input
   in snd $
        foldl
          ( \(state, count) newInstruction ->
              case newInstruction of
                Enable -> (Enable, count)
                Disable -> (Disable, count)
                Mul (x, y) -> (state, if state == Enable then count + (x * y) else count)
          )
          (Enable, 0)
          instructions

printDayThreeOne :: String -> IO ()
printDayThreeOne input = print ((runParsers <&> solve) (input ++ "xxxxxxxxxxxxxx"))

printDayThreeTwo :: String -> IO ()
printDayThreeTwo input = print (solve2 (input ++ "xxxxxxxxxxxxxx"))
