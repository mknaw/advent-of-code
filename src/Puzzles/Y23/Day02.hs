module Puzzles.Y23.Day02
  ( day02aSolve,
    day02bSolve,
  )
where

import qualified Data.List as L
import qualified Data.Text as T
import Lib.Parse
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char

data Game = Game
  { _id :: Int,
    _draws :: [Draw]
  } deriving (Show)

data Color = Red | Green | Blue

data Draw = Draw
  { _red :: Int,
    _green :: Int,
    _blue :: Int
  } deriving (Show)

day02aSolve :: PuzzleSolve [Game] Int
day02aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day02a
    }

day02bSolve :: PuzzleSolve [Game] Int
day02bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day02b
    }

parse' :: T.Text -> [Game]
parse' = parseInput $ parseGame `sepEndBy` newline
  where
    parseGame = do
      string "Game "
      id' <- parseInt
      string ": "
      draws <- parseDraw `sepBy` string "; "
      return Game {_id = id', _draws = draws}

    parseDraw = L.foldl' f d0 <$> parseColor `sepBy` string ", "
      where
        d0 = Draw {_red = 0, _green = 0, _blue = 0}
        f :: Draw -> (Color, Int) -> Draw
        f d (color, cubes) = case color of
          Red -> d {_red = _red d + cubes}
          Green -> d {_green = _green d + cubes}
          Blue -> d {_blue = _blue d + cubes}

    parseColor :: Parser (Color, Int)
    parseColor = do
      cubes <- parseInt
      space
      color <-
        choice
          [ Red <$ string "red",
            Green <$ string "green",
            Blue <$ string "blue"
          ]
      return (color, cubes)

day02a :: [Game] -> Int
day02a = sum . map _id . filter isViable
  where
    isViable :: Game -> Bool
    isViable = all f . _draws
      where
        f :: Draw -> Bool
        f d = _red d <= 12 && _green d <= 13 && _blue d <= 14

day02b :: [Game] -> Int
day02b = sum . fmap (power . minViable . _draws)
  where
    minViable :: [Draw] -> Draw
    minViable = L.foldl1' f
      where
        f :: Draw -> Draw -> Draw
        f d1 d2 = Draw {_red = r, _green = g, _blue = b}
          where
            r = max (_red d1) (_red d2)
            g = max (_green d1) (_green d2)
            b = max (_blue d1) (_blue d2)

    power :: Draw -> Int
    power Draw { _red = r, _green = g, _blue = b } = r * g * b
