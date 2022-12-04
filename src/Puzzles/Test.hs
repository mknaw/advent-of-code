module Puzzles.Test
  ( runTestCase,
    showTestResult,
    TestCase (..),
    testInputExists,
    testPath,
    wasSuccess,
  )
where

import qualified Data.List as L
import qualified Data.Text as T
import Lib.Console
import Puzzles.Puzzles
import System.Directory
import System.FilePath

data TestCase = TestCase
  { _input :: T.Text,
    _expected :: String
  }

data TestResult = TestResult
  { _test :: TestCase,
    _actual :: String
  }

testPath :: PuzzleSpec -> FilePath
testPath (PuzzleSpec day part) = "data/test" </> show day ++ show part <.> "txt"

testInputExists :: PuzzleSpec -> IO Bool
testInputExists spec = doesFileExist (testPath spec)

wasSuccess :: TestResult -> Bool
wasSuccess (TestResult tc actual) = actual == _expected tc

runTestCase :: SomeSolution -> TestCase -> TestResult
runTestCase solution tc = TestResult tc actual
  where
    actual = applySolution solution (_input tc)

showTestResult :: TestResult -> IO ()
showTestResult res
  | wasSuccess res = green $ putStrLn "✔"
  | otherwise =
    red . putStrLn $
      L.foldl1'
        (<>)
        [ "✗ Expected: ",
          _expected (_test res),
          ", Actual: ",
          _actual res
        ]
