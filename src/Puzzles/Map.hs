module Puzzles.Map
  ( getPuzzleSolution,
  )
where

import Puzzles.Day01
import Puzzles.Day02
import Puzzles.Day03
import Puzzles.Day04
import Puzzles.Day05
import Puzzles.Day06
import Puzzles.Day07
import Puzzles.Day08
import Puzzles.Day09
import Puzzles.Day10
import Puzzles.Day11
import Puzzles.Day12
import Puzzles.Day13
import Puzzles.Day14
import Puzzles.Day15
import Puzzles.Day16
import Puzzles.Day17
import Puzzles.Day18
import Puzzles.Day19
import Puzzles.Day20
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
getPuzzleSolution (PuzzleSpec (Day 4) PartA) = MkSomeSolution day04aSolve
getPuzzleSolution (PuzzleSpec (Day 4) PartB) = MkSomeSolution day04bSolve
getPuzzleSolution (PuzzleSpec (Day 5) PartA) = MkSomeSolution day05aSolve
getPuzzleSolution (PuzzleSpec (Day 5) PartB) = MkSomeSolution day05bSolve
getPuzzleSolution (PuzzleSpec (Day 6) PartA) = MkSomeSolution day06aSolve
getPuzzleSolution (PuzzleSpec (Day 6) PartB) = MkSomeSolution day06bSolve
getPuzzleSolution (PuzzleSpec (Day 7) PartA) = MkSomeSolution day07aSolve
getPuzzleSolution (PuzzleSpec (Day 7) PartB) = MkSomeSolution day07bSolve
getPuzzleSolution (PuzzleSpec (Day 8) PartA) = MkSomeSolution day08aSolve
getPuzzleSolution (PuzzleSpec (Day 8) PartB) = MkSomeSolution day08bSolve
getPuzzleSolution (PuzzleSpec (Day 9) PartA) = MkSomeSolution day09aSolve
getPuzzleSolution (PuzzleSpec (Day 9) PartB) = MkSomeSolution day09bSolve
getPuzzleSolution (PuzzleSpec (Day 10) PartA) = MkSomeSolution day10aSolve
getPuzzleSolution (PuzzleSpec (Day 10) PartB) = MkSomeSolution day10bSolve
getPuzzleSolution (PuzzleSpec (Day 11) PartA) = MkSomeSolution day11aSolve
getPuzzleSolution (PuzzleSpec (Day 11) PartB) = MkSomeSolution day11bSolve
getPuzzleSolution (PuzzleSpec (Day 12) PartA) = MkSomeSolution day12aSolve
getPuzzleSolution (PuzzleSpec (Day 12) PartB) = MkSomeSolution day12bSolve
getPuzzleSolution (PuzzleSpec (Day 13) PartA) = MkSomeSolution day13aSolve
getPuzzleSolution (PuzzleSpec (Day 13) PartB) = MkSomeSolution day13bSolve
getPuzzleSolution (PuzzleSpec (Day 14) PartA) = MkSomeSolution day14aSolve
getPuzzleSolution (PuzzleSpec (Day 14) PartB) = MkSomeSolution day14bSolve
getPuzzleSolution (PuzzleSpec (Day 15) PartA) = MkSomeSolution day15aSolve
getPuzzleSolution (PuzzleSpec (Day 15) PartB) = MkSomeSolution day15bSolve
getPuzzleSolution (PuzzleSpec (Day 16) PartA) = MkSomeSolution day16aSolve
getPuzzleSolution (PuzzleSpec (Day 16) PartB) = MkSomeSolution day16bSolve
getPuzzleSolution (PuzzleSpec (Day 17) PartA) = MkSomeSolution day17aSolve
getPuzzleSolution (PuzzleSpec (Day 17) PartB) = MkSomeSolution day17bSolve
getPuzzleSolution (PuzzleSpec (Day 18) PartA) = MkSomeSolution day18aSolve
getPuzzleSolution (PuzzleSpec (Day 18) PartB) = MkSomeSolution day18bSolve
getPuzzleSolution (PuzzleSpec (Day 19) PartA) = MkSomeSolution day19aSolve
getPuzzleSolution (PuzzleSpec (Day 19) PartB) = MkSomeSolution day19bSolve
getPuzzleSolution (PuzzleSpec (Day 20) PartA) = MkSomeSolution day20aSolve
getPuzzleSolution (PuzzleSpec (Day 20) PartB) = MkSomeSolution day20bSolve
getPuzzleSolution _ = error "puzzle for spec unavailable!"  -- TODO `Show` spec?
