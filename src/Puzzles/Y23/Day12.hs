{-# LANGUAGE DeriveGeneric, TypeOperators, TypeFamilies #-}

module Puzzles.Y23.Day12
  ( day12aSolve,
    day12bSolve,
  )
where

import qualified Data.List as L
import Data.MemoTrie
import qualified Data.Text as T
import Lib.Parse
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char
import GHC.Generics (Generic)

data Spring = Good | Bad | Wildcard deriving (Generic, Eq)

instance HasTrie Spring where
  newtype (Spring :->: b) = SpringTrie { unSpringTrie :: Reg Spring :->: b } 
  trie = trieGeneric SpringTrie 
  untrie = untrieGeneric unSpringTrie
  enumerate = enumerateGeneric unSpringTrie

instance Show Spring where
  show Good = "."
  show Bad = "#"
  show Wildcard = "?"

day12aSolve :: PuzzleSolve [([Spring], [Int])] Int
day12aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day12
    }

day12bSolve :: PuzzleSolve [([Spring], [Int])] Int
day12bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day12 . fmap expand
    }

parse' :: T.Text -> [([Spring], [Int])]
parse' = parseInput $ parseLine `sepEndBy` newline
  where
    parseLine :: Parser ([Spring], [Int])
    parseLine = do
      springs <- fmap parseSpring <$> some (char '#' <|> char '.' <|> char '?')
      hspace
      groups <- parseInt `sepBy` char ','
      return (springs, groups)

    parseSpring :: Char -> Spring
    parseSpring '.' = Good
    parseSpring '#' = Bad
    parseSpring '?' = Wildcard
    parseSpring _ = error "malformed input data"

countCombos :: [Spring] -> [Int] -> Int
countCombos s [] = if Bad `notElem` s then 1 else 0
countCombos [] _ = 0
countCombos s (g : gs) = nextCount' + nextCount
  where
    (s', rest) = L.splitAt g s
    valid = (length s' == g) && (Good `notElem` s') && (null rest || head rest /= Bad)
    nextCount =
      if head s == Bad
        then 0
        else countCombos' (tail s) (g : gs)
    nextCount' =
      if valid
        then countCombos' (drop 1 rest) gs
        else 0

countCombos' :: [Spring] -> [Int] -> Int
countCombos' = memo2 countCombos

day12 :: [([Spring], [Int])] -> Int
day12 = sum . fmap (uncurry countCombos)

expand :: ([Spring], [Int]) -> ([Spring], [Int])
expand (s, g) = (L.intercalate [Wildcard] . replicate 5 $ s, concat . replicate 5 $ g)
