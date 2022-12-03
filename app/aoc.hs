module Main where

import Control.Monad (forM_)
import qualified Data.Text.IO as T
import Lib.Parse (parseInput, parseTestCases)
import Options.Applicative
import Puzzles.Map
import Puzzles.Puzzles
import qualified System.Console.ANSI as ANSI

readDay :: ReadM Day
readDay = eitherReader $ \d -> Right . Day $ (read d :: Int)

-- TODO should allow running both?
readPart :: ReadM PuzzlePart
readPart = eitherReader f
  where
    f "a" = Right PartA
    f "b" = Right PartB
    f _ = Left "Invalid part"

-- TODO add an opt to skip tests?
problemSpecParser :: Parser PuzzleSpec
problemSpecParser =
  PuzzleSpec
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
            <> help "Run PART"
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
  ps <- execParser $ info (problemSpecParser <**> helper) fullDesc
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
    else withColor ANSI.Red (putStrLn "Tests failed :(")
  return ()
