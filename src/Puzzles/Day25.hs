module Puzzles.Day25
  ( day25aSolve,
    day25bSolve,
  )
where

import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import Puzzles.Puzzles

snafuChars :: String
snafuChars = "=-012"

-- TODO could look at using bounded integer type.
newtype Snafu = Snafu [Int]

instance Semigroup Snafu where
  (Snafu a) <> (Snafu b) = Snafu (a <> b)

instance Monoid Snafu where
  mempty = Snafu []

instance Show Snafu where
  show (Snafu []) = ""
  show (Snafu (x : xs)) = charFor x : show (Snafu xs)
    where
      charFor y = snafuChars !! (y + 2)

day25aSolve :: PuzzleSolve [Snafu] String
day25aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day25a
    }

day25bSolve :: PuzzleSolve [Snafu] String
day25bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day25b
    }

parse' :: T.Text -> [Snafu]
parse' = fmap Snafu . M.fromJust . mapM (mapM parse) . lines . T.unpack
  where
    parse :: Char -> Maybe Int
    parse c = subtract 2 <$> L.elemIndex c snafuChars

decodeSnafu :: Snafu -> Int
decodeSnafu (Snafu s) = foldl (\acc x -> acc * 5 + x) 0 s

-- TODO feels like these calculations should be cached
maxSnafuFor :: Int -> Int
maxSnafuFor d = decodeSnafu (Snafu (replicate d 2))

canFit :: Int -> Int -> Bool
canFit x d = abs x <= maxSnafuFor d

encodeSnafu :: Int -> Snafu
encodeSnafu 0 = mempty
encodeSnafu x = Snafu [s] <> Snafu (padTo (d - 1) r')
  where
    d = head . filter (canFit x) $ [1 ..]
    candidates = [- 2 .. 2]
    remainders = (x -) . (* (5 ^ (d - 1))) <$> candidates
    (s, r) = head . filter (flip canFit (d - 1) . snd) $ zip candidates remainders
    Snafu r' = encodeSnafu r

    padTo :: Int -> [Int] -> [Int]
    padTo n xs = replicate (n - length xs) 0 <> xs

day25a :: [Snafu] -> String
day25a = show . encodeSnafu . sum . fmap decodeSnafu

day25b :: [Snafu] -> String
day25b = undefined
