import Test.HUnit
import Prelude
import qualified System.Exit as Exit

test_foo :: Assertion
test_foo = do
  let x = 1
  assertEqual "foo" 1 1

test_bar :: Assertion
test_bar = do
  let x = 1
  assertEqual "bar" 1 1

main :: IO ()
main = do
  let tests =
        TestList
          [ TestCase test_foo,
            TestCase test_bar
          ]
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
