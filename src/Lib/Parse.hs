module Lib.Parse
  ( Parser,
    parseInput,
    parseInt,
    parseIntLines,
    parseTestCase,
    parseTestCases,
  )
where

import Data.Text hiding (show)
import Data.Void
import Lib.Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import Puzzles.Test
import Control.Monad (void)

type Parser = Parsec Void Text

parseInput :: Parser a -> Text -> a
parseInput parser input =
  case runParser parser "" input of
    Left err -> error $ show err
    Right x -> x

parseInt :: Parser Int
parseInt = signed space decimal

parseIntLines :: Parser [Int]
parseIntLines = parseInt `sepEndBy` eol

parseTestCase :: Parser TestCase
parseTestCase = do
  input <- someTill anySingle (try $ string "\n>>> ")
  -- There probably are some smoother ways to do this...
  output <- someTill anySingle (try $ void (try $ string "\n\n") <|> (char '\n' *> eof))
  return $ TestCase (pack input) (trim output)

parseTestCases :: Parser [TestCase]
parseTestCases = many parseTestCase
