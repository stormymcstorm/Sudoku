{-# LANGUAGE NamedFieldPuns #-}
module Sudoku.Board where

import Data.Map(Map(..))
import qualified Data.Map as Map
import Data.IntSet(IntSet(..))
import qualified Data.IntSet as IntSet
import Utils (groupIntoN)
import Data.List (transpose, groupBy)
import Control.Monad (liftM2)
import Data.Maybe (mapMaybe)

-- | Represents a sudoku board
data Board = Board {
  dim :: Dim, 
  candid :: Candidates,
  cells :: Map Pos Int
}

-- | Creates an empty sudoku board with the given dimension
empty :: Dim -> Board
empty dim@(m,n) = fromList dim (replicate ((m * n)^2) 0) 

-- | Sets cell at the given position to the given value
set :: Pos -> Int -> Board -> Board
set p v brd@Board{candid, cells} = 
  brd{candid = takeCandidate p v candid, cells = Map.insert p v cells}

-- | Returns a list of all possible values a cell can contain
viable :: Pos -> Board -> [Int]
viable p brd@Board{cells, candid} = case Map.lookup p cells of
  Nothing -> error ("Invalid position " ++ show p)
  Just 0 -> IntSet.toList $ getCandidates p candid
  Just v -> [v]

-- | Creates a new board from a dimension and a list of values
fromList :: Dim -> [Int] -> Board
fromList dim = 
  foldl (flip . uncurry $ set) (Board dim (mkCandidates dim) Map.empty) . zip (positions dim)

-- | An example board 
example :: Board
example = fromList (3,2) [
  -- 6, 0, 9,   0, 2, 0,   0, 0, 8,
  -- 0, 8, 0,   5, 0, 6,   0, 0, 9,
  -- 4, 0, 0,   0, 0, 0,   0, 2, 0,

  -- 0, 0, 0,   0, 0, 3,   0, 0, 0,
  -- 9, 0, 0,   0, 0, 0,   0, 0, 4,
  -- 0, 0, 0,   4, 6, 0,   8, 3, 1,

  -- 0, 0, 0,   0, 1, 0,   0, 7, 6,
  -- 0, 0, 7,   0, 0, 0,   3, 9, 0,
  -- 0, 0, 0,   0, 7, 0,   0, 0, 0

  1, 0, 3,  4, 5, 6,
  0, 0, 0,  1, 0, 3,

  2, 3, 4,  5, 6, 1,
  0, 0, 0,  0, 0, 0,

  3, 0, 5,  6, 0, 2,
  6, 1, 0,  3, 0, 5
  ]

-- | Converts a board to a list of values
toList :: Board -> [Int]
toList = map snd . Map.toAscList . cells  

-- | Convert the board into a list of rows
rows :: Board -> [[Int]]
rows brd@Board{dim=(m, n)} =  groupIntoN (m * n) . map snd . Map.toAscList . cells $ brd
  where sameRow ((_, r), _) ((_, r'), _) = r == r'

-- | Convert the board into a list of columns
cols :: Board -> [[Int]]
cols =  transpose . rows

-- | Serializes a board
serialize :: Board -> String
serialize brd@Board{dim} = 
  show dim ++ ":" ++ show (toList brd)

-- | Deserializes a board
deserialize :: String -> Board
deserialize s = fromList dim values
  where 
    values = read _values :: [Int]
    dim = read _dim :: Dim
    (_dim, ':':_values) = break (==':') s

-- | Represents the dimensions of a sudoku board m and n
type Dim = (Int, Int)

-- | Represents a position on the sudoku board
type Pos = (Int, Int)

positions :: Dim -> [Pos]
positions (m,n) = (,) <$> [0..m * n - 1] <*> [0..m * n - 1]

-- | Identifies a section of the board, sections come in three types rows, columns
-- and regions. Each section is numbered from `0..m*n - 1` for example for a (3,3)
-- board the sections are numbered
-- 
--    0 1 2   3 4 5   6 7 8
--  0 . . . | . . . | . . .
--  1 . 0 . | . 1 . | . 2 . 
--  2 . . . | . . . | . . .
--    ------+-------+------
--  3 . . . | . . . | . . .
--  4 . 3 . | . 4 . | . 5 .
--  5 . . . | . . . | . . .
--    ------+-------+------
--  6 . . . | . . . | . . .
--  7 . 6 . | . 7 . | . 8 .
--  8 . . . | . . . | . . .
-- 
data BoardSection = Row Int | Col Int | Region Int deriving (Eq, Ord, Show)

-- | Manages the available candidates for each section
data Candidates = Candidates {bdim :: Dim, aval :: Map BoardSection IntSet} deriving Show

mkCandidates :: Dim -> Candidates
mkCandidates dim@(m,n) = 
  let
    all_candidates = IntSet.fromList [1..m * n]
  in
    Candidates dim . Map.fromList $ [0..m * n - 1] >>= \i -> [
      (Row i, all_candidates), (Col i, all_candidates), (Region i, all_candidates)
    ]

region :: Dim -> Pos -> Int
region (m,n) (row, col)= 
  let
    rrow = row `div` n
    rcol = col `div` m
    region = rrow * n + rcol
  in region

takeCandidate :: Pos -> Int -> Candidates -> Candidates
takeCandidate p@(row, col) v cans@Candidates{bdim, aval} =
  let 
    sections = [Row row, Col col, Region (region bdim p)]
    update vs = if IntSet.null nvs then Nothing else Just nvs
      where nvs = IntSet.delete v vs
  in
    cans{aval = foldl (flip (Map.update update)) aval sections}

getCandidates :: Pos ->  Candidates -> IntSet
getCandidates p@(row, col) Candidates{bdim, aval} = 
  let
    scans :: [IntSet]
    scans = mapMaybe (`Map.lookup` aval) [Row row, Col col, Region (region bdim p)]
  in
    case scans of
      [] -> IntSet.empty
      _ -> foldr1 IntSet.intersection scans