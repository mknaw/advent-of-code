module Puzzles.Map
  ( getPuzzleSolution,
  )
where

import Puzzles.Day01
import Puzzles.Day02
import Puzzles.Day03
-- import Puzzles.Day04
-- import Puzzles.Day05
-- import Puzzles.Day06
-- import Puzzles.Day07
-- import Puzzles.Day08
-- import Puzzles.Day09
-- import Puzzles.Day10
-- import Puzzles.Day11
-- import Puzzles.Day12
-- import Puzzles.Day13
-- import Puzzles.Day14
-- import Puzzles.Day15
-- import Puzzles.Day16
-- import Puzzles.Day17
-- import Puzzles.Day18
-- import Puzzles.Day19
-- import Puzzles.Day20
-- import Puzzles.Day21
-- import Puzzles.Day22
-- import Puzzles.Day23
-- import Puzzles.Day24
-- import Puzzles.Day25
import Puzzles.Puzzles

-- TODO figure out some programmatic way to do this? Maybe TH?
getPuzzleSolution :: PuzzleSpec -> SomeSolution
getPuzzleSolution (PuzzleSpec (Day 1) PartA) = MkSomeSolution day01aSolve
getPuzzleSolution (PuzzleSpec (Day 1) PartB) = MkSomeSolution day01bSolve
getPuzzleSolution (PuzzleSpec (Day 2) PartA) = MkSomeSolution day02aSolve
getPuzzleSolution (PuzzleSpec (Day 2) PartB) = MkSomeSolution day02bSolve
getPuzzleSolution (PuzzleSpec (Day 3) PartA) = MkSomeSolution day03aSolve
getPuzzleSolution (PuzzleSpec (Day 3) PartB) = MkSomeSolution day03bSolve
getPuzzleSolution _ = error "puzzle for spec unavailable!"  -- TODO `Show` spec?
