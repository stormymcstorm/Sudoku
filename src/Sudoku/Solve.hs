{-# LANGUAGE NamedFieldPuns #-}
module Sudoku.Solve where

import Data.Map(Map(..))
import qualified Data.Map as Map
import Control.Monad.State
import Sudoku.Board(Board(..))
import qualified Sudoku.Board as Board

solve :: Board -> [Board]
solve brd@Board{dim} = solve' (Board.positions dim) brd

solve' :: [Board.Pos] -> Board -> [Board]
solve' positions = execStateT (mapM_ setNext positions)

setNext :: Board.Pos -> StateT Board [] ()
setNext pos = do
  brd@Board{cells} <- get
  val <- lift $ Board.viable pos brd
  put $ Board.set pos val brd