module Lib.Parse
  ( Parser,
    parseInput,
    parseInt,
    parseIntLines,
    parseTestCase,
    parseTestCases,
  )
where

import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Puzzles.Test

type Parser = Parsec Void Text

parseInput :: Parser a -> Text -> a
parseInput parser input =
  case runParser parser "" input of
    Left err -> error $ show err
    Right x -> x

parseInt :: Parser Int
parseInt = read <$> some digitChar

parseIntLines :: Parser [Int]
parseIntLines = parseInt `sepEndBy` eol

parseTestCase :: Parser TestCase
parseTestCase = do
  input <- someTill anySingle (try $ string "\n>>> ")
  output <- someTill anySingle newline
  -- TODO why does it think I wanted a `String` `input` and not `Text`?
  return $ TestCase (pack input) output

parseTestCases :: Parser [TestCase]
parseTestCases = parseTestCase `sepEndBy` char '\n'
