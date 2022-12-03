module Main where

import Control.Monad
import qualified Data.Text.IO as T
import Lib.Parse (parseInput, parseTestCases)
import Options.Applicative
import Puzzles.Map
import Puzzles.Puzzles
import qualified System.Console.ANSI as ANSI

data RunOpts = RunOpts
  { _day :: Day,
    _part :: PuzzlePart,
    _bench :: Bool
  }

puzzleSpecFrom :: RunOpts -> PuzzleSpec
puzzleSpecFrom (RunOpts day part _) = PuzzleSpec day part

readDay :: ReadM Day
readDay = eitherReader $ \d -> Right . Day $ (read d :: Int)

-- TODO should allow running both?
readPart :: ReadM PuzzlePart
readPart = eitherReader f
  where
    f "a" = Right PartA
    f "b" = Right PartB
    f _ = Left "Invalid part"

runOptsParser :: Parser RunOpts
runOptsParser =
  RunOpts
    <$> option
      readDay
      ( long "day"
          <> short 'd'
          <> help "Which day to solve"
          <> metavar "INT"
      )
      <*> option
        readPart
        ( long "part"
            <> short 'p'
            <> metavar "PART"
            <> help "Run PART - should be 'a' or 'b'"
        )
      <*> switch
        ( long "bench"
            <> short 'b'
            <> help "Whether to run benchmarks"
        )

withColor :: ANSI.Color -> IO () -> IO ()
withColor color act = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid color]
  act
  ANSI.setSGR [ANSI.Reset]

runTestCase :: SomeSolution -> TestCase -> Bool
runTestCase solution (TestCase input expected) =
  let actual = applySolution solution input
   in actual == expected

runTests :: PuzzleSpec -> SomeSolution -> IO Bool
runTests ps solution = do
  putStr "Running tests..."
  testInput <- T.readFile $ testPath ps
  let testCases = parseInput parseTestCases testInput
  let results = runTestCase solution <$> testCases
  forM_ results $ \res ->
    if res
      then withColor ANSI.Green (putStr " ✔")
      else -- TODO would be good to show the actual vs expected for failures
        withColor ANSI.Red (putStr " ✗")
  putStrLn ""
  return $ and results

main :: IO ()
main = do
  runOpts <- execParser $ info (runOptsParser <**> helper) fullDesc
  let ps = puzzleSpecFrom runOpts
  let solution = getPuzzleSolution ps
  testsPass <-
    testInputExists ps >>= \case
      True -> runTests ps solution
      False -> do
        putStrLn "No tests found"
        return True
  if testsPass
    then do
      input <- readInput ps
      let solved = applySolution solution input
      putStrLn $ show ps <> ": " <> solved
      when (_bench runOpts) $ benchmarkSolution solution input
    else withColor ANSI.Red (putStrLn "Tests failed :(")
  return ()
