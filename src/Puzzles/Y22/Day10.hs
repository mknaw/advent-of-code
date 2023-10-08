module Puzzles.Y22.Day10
  ( day10aSolve,
    day10bSolve,
  )
where

import Control.Monad.State
import qualified Data.Text as T
import Lib.Parse
import Lib.Utils
import Puzzles.Puzzles
import Text.Megaparsec hiding (Pos, State)
import Text.Megaparsec.Char
import Prelude hiding (cycle)
import Data.List.Split (chunksOf)

day10aSolve :: PuzzleSolve [Instr] Int
day10aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day10a
    }

day10bSolve :: PuzzleSolve [Instr] String
day10bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day10b
    }

data Instr = Noop | Add Int

parse' :: T.Text -> [Instr]
parse' = parseInput $ parser `sepEndBy` eol
  where
    parser :: Parser Instr
    parser =
      choice
        [ Noop <$ string "noop",
          Add <$> (string "addx " *> parseInt)
        ]

type RState = ([Int], Int)

mkRState :: RState
mkRState = ([], 1)

execAll :: [Instr] -> [Int]
execAll ins = reverse . fst $ execState (go ins) mkRState
  where
    go :: [Instr] -> State RState ()
    go [] = return ()
    go (Noop : xs) = do
      (rs, r) <- get
      put (r : rs, r)
      go xs
    go (Add x : xs) = do
      (rs, r) <- get
      put (r : r : rs, r + x)
      go xs

day10a :: [Instr] -> Int
day10a ins = sum . zipWith (*) grid $ samples
  where
    grid = [20, 60, 100, 140, 180, 220]
    states = execAll ins
    samples = map (\i -> states !! (i - 1)) grid

printer :: Int -> Int -> Char
printer cyc reg
  | (cyc `rem` 40) `elem` [reg - 1 .. reg + 1] = '#'
  | otherwise = '.'

day10b :: [Instr] -> String
day10b ins = trim . unlines . chunksOf 40 . zipWith printer [0 ..] $ execAll ins
