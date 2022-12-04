module Lib.Console
  ( green,
    red,
    withColor,
  )
where

import qualified System.Console.ANSI as ANSI

withColor :: ANSI.Color -> IO () -> IO ()
withColor color act = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid color]
  act
  ANSI.setSGR [ANSI.Reset]

green :: IO () -> IO ()
green = withColor ANSI.Green

red :: IO () -> IO ()
red = withColor ANSI.Red
