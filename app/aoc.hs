module Main where

import Control.Monad
import qualified Data.Text.IO as T
import Lib.Console
import Lib.Parse (parseInput, parseTestCases)
import Options.Applicative
import Puzzles.Map
import Puzzles.Puzzles
import Puzzles.Test

data RunOpts = RunOpts
  { _puzzleSpec :: PuzzleSpec,
    _bench :: Bool,
    _skipTests :: Bool
  }

readDay :: ReadM Day
readDay = eitherReader $ \d -> Right . Day $ (read d :: Int)

-- TODO should allow running both?
readPart :: ReadM PuzzlePart
readPart = eitherReader f
  where
    f "a" = Right PartA
    f "b" = Right PartB
    f _ = Left "Invalid part"

puzzleSpecParser :: Parser PuzzleSpec
puzzleSpecParser =
  PuzzleSpec
    <$> option
      readDay
      ( long "day"
          <> short 'd'
          <> metavar "DAY"
          <> help "Day to run"
      )
    <*> option
      readPart
      ( long "part"
          <> short 'p'
          <> metavar "PART"
          <> help "Part to run"
      )

runOptsParser :: Parser RunOpts
runOptsParser = do
  _puzzleSpec <- puzzleSpecParser
  _bench <-
    switch
      ( long "bench"
          <> short 'b'
          <> help "Whether to run benchmarks"
      )
  _skipTests <-
    switch
      ( long "skip-tests"
          <> short 'T'
          <> help "Skip tests"
      )
  return $ RunOpts _puzzleSpec _bench _skipTests

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
  RunOpts ps doBench skipTests <- execParser $ info (runOptsParser <**> helper) fullDesc
  let solution = getPuzzleSolution ps
  testInputExists' <- testInputExists ps
  shouldRun <-
    if testInputExists' && not skipTests
      then runTests ps solution
      else return True
  if shouldRun
    then do
      input <- readInput ps
      let solved = applySolution solution input
      putStrLn $ show ps <> ": " <> solved
      when doBench $ benchmarkSolution solution input
    else red $ putStrLn "Tests failed :("
  return ()
