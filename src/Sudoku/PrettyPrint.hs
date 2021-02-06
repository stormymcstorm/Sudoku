module Sudoku.PrettyPrint where

import qualified Text.PrettyPrint.Boxes as PB

import Sudoku.Board(Board, cols)

instance Show Board where
  show = 
    PB.render 
    . PB.hsep 1 PB.left
    . map (PB.vcat PB.left . map (PB.text . valToString))
    . cols
    where
      valToString 0 = "."
      valToString x = show x