{-# LANGUAGE FlexibleContexts #-}

module Lib.Parse
  ( Parser,
    parseDigit,
    parseDigitLines,
    parseInput,
    parseInt,
    parseIntLines,
    parseTestCase,
    parseTestCases,
  )
where

import Control.Monad (void)
import Data.Text hiding (show)
import Data.Void
import Lib.Utils
import Puzzles.Test
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

type Parser = Parsec Void Text

parseInput :: (Stream s, Show s, Show (Token s)) => Parsec Void s a -> s -> a
parseInput parser input =
  case runParser parser mempty input of
    Left err -> error $ show err
    Right x -> x

parseInt :: Parser Int
parseInt = signed space decimal

parseIntLines :: Parser [Int]
parseIntLines = parseInt `sepEndBy` eol

parseDigit :: Parser Int
parseDigit = read . (: []) <$> digitChar

parseDigitLines :: Parser [[Int]]
parseDigitLines = (some parseDigit) `sepEndBy` eol

parseTestCase :: Parser TestCase
parseTestCase = do
  input <- someTill anySingle (try $ string "\n>>> ")
  -- There probably are some smoother ways to do this...
  output <- someTill anySingle (try $ void (try $ string "\n\n") <|> (char '\n' *> eof))
  return $ TestCase (pack input) (trim output)

parseTestCases :: Parser [TestCase]
parseTestCases = many parseTestCase
