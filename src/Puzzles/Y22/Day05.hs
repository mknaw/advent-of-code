module Puzzles.Y22.Day05
  ( day05aSolve,
    day05bSolve,
  )
where

import Control.Monad (foldM, (<=<))
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as M
import Data.Nat
import qualified Data.Text as T
import Lib.Parse
import Lib.Utils
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char

type Cargo = M.Map Nat String

data Move = Move Nat Nat Nat

type Scenario = (Cargo, [Move])

day05 :: (Cargo -> Move -> Maybe Cargo) -> Scenario -> String
day05 f = M.fromJust . (tops <=< uncurry (foldM f))

day05aSolve :: PuzzleSolve Scenario String
day05aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day05 moveOne
    }

day05bSolve :: PuzzleSolve Scenario String
day05bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day05 moveMany
    }

parse' :: T.Text -> Scenario
parse' = parseInput $ do
  cargo <- makeCargo <$> (parseCargoCell `sepBy` char ' ') `sepBy` newline
  some $ digitChar <|> char ' '
  some newline
  moves <- parseMove `sepEndBy1` newline
  return (cargo, moves)
  where
    parseCargoCell :: Parser (Maybe Char)
    parseCargoCell = emptyCell <|> cargoCell
      where
        emptyCell = Nothing <$ string "   "
        cargoCell = do
          char '['
          cell <- upperChar
          char ']'
          return $ Just cell

    makeCargo :: [[Maybe Char]] -> Cargo
    makeCargo css = M.fromAscList $ zip [1 ..] values
      where
        values = map M.catMaybes . L.transpose $ css

    parseMove :: Parser Move
    parseMove = do
      string "move "
      qty <- fromInteger . read <$> some digitChar
      string " from "
      from <- fromInteger . read <$> some digitChar
      string " to "
      to <- fromInteger . read <$> some digitChar
      return $ Move qty from to

pop :: String -> Maybe (String, Char)
pop [] = Nothing
pop (c : cs) = Just (cs, c)

peek :: String -> Maybe Char
peek = fmap snd . pop

tops :: Cargo -> Maybe String
tops = fmap reverse . foldM (\s st -> (:) <$> peek st <*> pure s) ""

moveOne :: Cargo -> Move -> Maybe Cargo
moveOne cargo (Move qty from to) = do
  bindN (move from to) qty cargo
  where
    move :: Nat -> Nat -> Cargo -> Maybe Cargo
    move f t c = do
      (source, val) <- pop $ c M.! f
      return $ M.adjust (val :) t $ M.insert f source c

moveMany :: Cargo -> Move -> Maybe Cargo
moveMany cargo (Move qty from to) = do
  (source, vals) <- bindN popMany qty (cargo M.! from, [])
  return $
    M.adjust (reverse vals ++) to
      . M.insert from source
      $ cargo
  where
    popMany :: (String, String) -> Maybe (String, String)
    popMany (s, cs) = do
      (s', c') <- pop s
      return (s', c' : cs)
