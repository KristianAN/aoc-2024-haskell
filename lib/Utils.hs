module Utils (parseAndPrint) where

import Text.Gigaparsec

parseAndPrint :: (Show a) => Parsec a -> String -> IO ()
parseAndPrint parser input = case parse @String parser input of
  Success res -> print res
  Failure err -> putStrLn err
