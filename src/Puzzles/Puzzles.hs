{-# LANGUAGE GADTs #-}

module Puzzles.Puzzles
  ( applySolution,
    Day (..),
    inputPath,
    mkPuzzleSpec,
    PuzzlePart (PartA, PartB),
    PuzzleSpec (..),
    readInput,
    PuzzleSolve (..),
    SomeSolution (..),
    TestCase (..),
    testInputExists,
    testPath,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath

data PuzzlePart = PartA | PartB

instance Show PuzzlePart where
  show PartA = "a"
  show PartB = "b"

-- TODO should be a NAT instead of an Int?
newtype Day = Day {_d :: Int}

instance Show Day where
  show (Day day)
    | day >= 10 = show day
    | otherwise = '0' : show day

data PuzzleSpec = PuzzleSpec
  { unDay :: Day,
    unPart :: PuzzlePart
  }

instance Show PuzzleSpec where
  show (PuzzleSpec day part) = "Day " <> show day <> ", Part " <> show part

mkPuzzleSpec :: Int -> PuzzlePart -> PuzzleSpec
mkPuzzleSpec day part = PuzzleSpec {unDay = Day day, unPart = part}

inputPath :: PuzzleSpec -> FilePath
inputPath (PuzzleSpec day _) = "data" </> show day <.> "txt"

testPath :: PuzzleSpec -> FilePath
testPath (PuzzleSpec day part) = "data/test" </> show day ++ show part <.> "txt"

testInputExists :: PuzzleSpec -> IO Bool
testInputExists spec = doesFileExist (testPath spec)

readInput :: PuzzleSpec -> IO T.Text
readInput ps = do
  T.readFile $ inputPath ps

data PuzzleSolve a b = PuzzleSolve
  { _parse :: T.Text -> a,
    _solve :: a -> b
  }

data SomeSolution where
  MkSomeSolution :: Show b => PuzzleSolve a b -> SomeSolution

applySolution :: SomeSolution -> T.Text -> String
applySolution (MkSomeSolution (PuzzleSolve parse solve)) input = show . solve $ parse input

data TestCase = TestCase
  { _input :: T.Text,
    -- TODO would prefer a `TestCase a` with generic solution type but don't know how (yet)
    _expected :: String
  }
