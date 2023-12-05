{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Puzzles.Puzzles
  ( applySolution,
    benchmarkSolution,
    Day (..),
    inputPath,
    mkPuzzleSpec,
    PuzzlePart (PartA, PartB),
    PuzzleSpec (..),
    readInput,
    PuzzleSolve (..),
    SomeSolution (..),
  )
where

import Criterion
import Criterion.Types (anMean, reportAnalysis)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Statistics.Types (estPoint)
import System.FilePath

class ToString a where
  toString :: a -> String

instance {-# OVERLAPPING #-} ToString String where
  toString s = s

instance Show a => ToString a where
  toString x = show x

data PuzzlePart = PartA | PartB deriving (Eq)

instance Show PuzzlePart where
  show PartA = "a"
  show PartB = "b"

-- TODO should be a NAT instead of an Int?
newtype Day = Day {_d :: Int} deriving (Eq, Ord)

instance Show Day where
  show (Day day)
    | day >= 10 = show day
    | otherwise = '0' : show day

data PuzzleSpec = PuzzleSpec
  { unYear :: Int,
    unDay :: Day,
    unPart :: PuzzlePart
  }

instance Show PuzzleSpec where
  show (PuzzleSpec year day part) =
    concat
      [ "Y",
        show year,
        ", Day ",
        show day,
        ", Part ",
        show part
      ]

mkPuzzleSpec :: Int -> PuzzlePart -> PuzzleSpec
mkPuzzleSpec day part = PuzzleSpec {unYear = 22, unDay = Day day, unPart = part}

inputPath :: PuzzleSpec -> FilePath
inputPath (PuzzleSpec year day _) = "data" </> ('Y' : show year) </> show day <.> "txt"

readInput :: PuzzleSpec -> IO T.Text
readInput ps = do
  T.readFile $ inputPath ps

data PuzzleSolve a b = PuzzleSolve
  { _parse :: T.Text -> a,
    _solve :: a -> b
  }

data SomeSolution where
  MkSomeSolution :: ToString b => PuzzleSolve a b -> SomeSolution

applySolution :: SomeSolution -> T.Text -> String
applySolution (MkSomeSolution (PuzzleSolve parse solve)) input = toString . solve $ parse input

benchmarkSolution :: SomeSolution -> T.Text -> IO Double
benchmarkSolution (MkSomeSolution (PuzzleSolve parse solve)) input = do
  let parsed = parse input
  estPoint . anMean . reportAnalysis <$> benchmark' (whnf solve parsed)
