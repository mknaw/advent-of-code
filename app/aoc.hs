{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Data.List.Split (splitOn)
import qualified Data.Text.IO as T
import Lib.Console
import Lib.Parse (parseInput, parseTestCases)
import Lib.Summary (writeBenchmark)
import Options.Applicative
import Puzzles.Map
import Puzzles.Puzzles
import Puzzles.Test

data RunOpts = RunOpts
  { _puzzleSpecs :: [PuzzleSpec],
    _bench :: Bool,
    _write :: Bool,
    _skipTests :: Bool
  }

-- Yes lol TODO on this fn.. but, it works until next year!
readYear :: ReadM Int
readYear = eitherReader $ \case
  "22" -> Right 22
  "23" -> Right 23
  "25" -> Right 25
  _ -> Left "Invalid year"

-- TODO probably want to actually return Nothing when they're bad
readDaySegment :: String -> Maybe [Day]
readDaySegment s = case splitOn "-" s of
  [d] -> Just [Day $ read d]
  [d1, d2] -> Just . fmap Day $ [read d1 .. read d2]
  _ -> Nothing

readDays :: ReadM [Day]
readDays = eitherReader $ \s ->
  let days = splitOn "," s
   in case traverse readDaySegment days of
        Just days' -> Right $ concat days'
        Nothing -> Left "Invalid day"

readPart :: ReadM [PuzzlePart]
readPart = eitherReader $ \case
  "a" -> Right [PartA]
  "b" -> Right [PartB]
  _ -> Left "Invalid part"

puzzleSpecParser :: Parser [PuzzleSpec]
puzzleSpecParser = do
  year <-
    option
      readYear
      ( long "year"
          <> short 'y'
          <> metavar "YEAR"
          <> help "Year to run"
      )
  days <-
    option
      readDays
      ( long "days"
          <> short 'd'
          <> metavar "DAYS"
          <> help "Days to run"
          <> value [Day 1]
      )
  parts <-
    option
      readPart
      ( long "parts"
          <> short 'p'
          <> metavar "PARTS"
          <> help "Parts to run (a or b or omit to run both)"
          <> value [PartA, PartB]
      )
  return [PuzzleSpec year day part | day <- days, part <- parts]

runOptsParser :: Parser RunOpts
runOptsParser = do
  _puzzleSpecs <- puzzleSpecParser
  _bench <-
    switch
      ( long "bench"
          <> short 'b'
          <> help "Whether to run benchmarks"
      )
  _write <-
    switch
      ( long "write"
          <> short 'w'
          <> help "Whether to write benchmarks to README table"
      )
  _skipTests <-
    switch
      ( long "skip-tests"
          <> short 'T'
          <> help "Skip tests"
      )
  return $ RunOpts _puzzleSpecs _bench _write _skipTests

runTests :: PuzzleSpec -> SomeSolution -> IO Bool
runTests ps solution = do
  putStrLn "Running tests..."
  testInput <- T.readFile $ testPath ps
  let testCases = parseInput parseTestCases testInput
  let results = runTestCase solution <$> testCases
  forM_ results showTestResult
  return $ and (wasSuccess <$> results)

main :: IO ()
main = do
  RunOpts {..} <- execParser $ info (runOptsParser <**> helper) fullDesc
  forM_ _puzzleSpecs $ \ps -> do
    let solution = getPuzzleSolution ps
    testInputExists' <- testInputExists ps
    shouldRun <-
      if testInputExists' && not _skipTests
        then runTests ps solution
        else return True
    if shouldRun
      then do
        input <- readInput ps
        let solved = applySolution solution input
        let spacer = if length (lines solved) > 1 then "\n" else ""
        putStrLn $ show ps <> ": " <> spacer <> solved
        when _bench $ do
          meantime <- benchmarkSolution solution input
          when _write $ writeBenchmark ps meantime
      else red $ putStrLn "Tests failed :("
    return ()
