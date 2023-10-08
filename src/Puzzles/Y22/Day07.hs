{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Puzzles.Y22.Day07
  ( day07aSolve,
    day07bSolve,
  )
where

import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as M
import qualified Data.Text as T
import Lib.Parse
import Puzzles.Puzzles
import Text.Megaparsec
import Text.Megaparsec.Char

data Directory = Directory
  { dirs :: M.Map String Directory,
    files :: [Int]
  }
  deriving (Show)

type DirectoryCursor = (Directory, [(String, Directory)])

makeBaseFunctor ''Directory

day07aSolve :: PuzzleSolve [Instruction] Int
day07aSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day07a
    }

day07bSolve :: PuzzleSolve [Instruction] Int
day07bSolve =
  PuzzleSolve
    { _parse = parse',
      _solve = day07b
    }

data CDTarget = Root | Up | Down String

data Instruction = CD CDTarget | File Int

parse' :: T.Text -> [Instruction]
parse' = parseInput $ M.catMaybes <$> parser `sepEndBy` eol
  where
    parser :: Parser (Maybe Instruction)
    parser =
      choice
        [ Just (CD Root) <$ string "$ cd /",
          Just (CD Up) <$ string "$ cd ..",
          Just . CD . Down <$> (string "$ cd " *> some printChar),
          Just . File <$> (read <$> some digitChar) <* (string " " *> some printChar),
          Nothing <$ (string "dir " *> some printChar),
          Nothing <$ string "$ ls"
        ]

mkDirectory :: Directory
mkDirectory = Directory mempty mempty

construct :: [Instruction] -> Directory
construct = fst . flip exec (CD Root) . foldl exec (mkDirectory, [])

exec :: DirectoryCursor -> Instruction -> DirectoryCursor
exec cur@(dir@Directory {..}, crumbs) = \case
  CD Root -> if null crumbs then cur else L.foldl' exec cur [CD Up, CD Root]
  CD Up -> (parent {dirs = M.insert name dir dirs'}, rest)
    where ((name, parent@(Directory dirs' _)), rest) = M.fromJust . L.uncons $ crumbs
  CD (Down name) -> (subDir, (name, dir) : crumbs)
    where subDir = M.findWithDefault mkDirectory name dirs
  File sz -> (dir {files = sz : files}, crumbs)

collect :: Directory -> [Int]
collect = cata $ \DirectoryF {..} ->
  (sum filesF + (sum . fmap head $ dirsF)) : concat dirsF

day07a :: [Instruction] -> Int
day07a = sum . filter (100000 >=) . collect . construct

day07b :: [Instruction] -> Int
day07b ins = minimum . filter (target <=) $ sizes
  where
    sizes = collect . construct $ ins
    target = head sizes - 40000000
