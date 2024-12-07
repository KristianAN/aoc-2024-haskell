module Utils (parseAndPrint, extract, diagonals) where

import Data.Foldable (Foldable (foldl'))
import Text.Gigaparsec

extract :: [Result e a] -> [a]
extract =
  foldl'
    ( \acc v -> case v of
        Success s -> s : acc
        Failure _ -> acc
    )
    []

parseAndPrint :: (Show a) => Parsec a -> String -> IO ()
parseAndPrint parser input = case parse @String parser input of
  Success res -> print res
  Failure err -> putStrLn err

forwardDiagonals :: Int -> [[a]] -> [[a]]
forwardDiagonals n matrix =
  [ [matrix !! (i + k) !! (j + k) | k <- [0 .. n - 1]]
    | i <- [0 .. length matrix - n],
      j <- [0 .. length (head matrix) - n]
  ]

backwardDiagonals :: Int -> [[a]] -> [[a]]
backwardDiagonals n matrix =
  [ [matrix !! (i + k) !! (j - k) | k <- [0 .. n - 1]]
    | i <- [0 .. length matrix - n],
      j <- [n - 1 .. length (head matrix) - 1]
  ]

-- Gets all diagonals of a matrix of n length. Does not check if the matrix is a valid matrix
diagonals :: Int -> [[a]] -> [[a]]
diagonals n matrix = forwardDiagonals n matrix ++ backwardDiagonals n matrix
