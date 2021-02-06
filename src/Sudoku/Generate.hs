module Sudoku.Generate where

import System.Random ( RandomGen )
import System.Random.Shuffle (shuffle')

import Sudoku.Board(Board(..))
import qualified Sudoku.Board as Board

import Sudoku.Solve (solve')

generate :: RandomGen g => Board.Dim -> Int -> g -> Board
generate dim clues gen = 
  let
    positions = Board.positions dim
    shuffled_positions = take clues (shuffle' positions (length positions) gen)
  in
    case solve' shuffled_positions (Board.empty dim) of
      [] -> error "Failed to generate board"
      b:br -> b