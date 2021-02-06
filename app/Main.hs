module Main where

import Control.Monad (join)
import System.IO
import System.Random

import Options.Applicative

import Sudoku.Board(Board)
import Sudoku.PrettyPrint
import qualified Sudoku.Board as Board
import qualified Sudoku.Solve as Solve
import qualified Sudoku.Generate as Generate


main :: IO ()
main = join $ execParser (info commands desc)
  where 
    desc = idm -- TODO: decs
    commands = subparser (genCommand <> solveCommand) <**> helper

genCommand = command "gen" (info parser desc)
  where
    parser = generate 
      <$> argument str (
        metavar "FILE"
        <> help "The board containing the serialized board"
      )
      <*> (option auto (short 'M'
        <> metavar "INT"
        <> value 3
        <> help "The number of columns in each block"
        <> showDefault
      ) :: Parser Int)
      <*> (option auto (short 'N'
        <> metavar "INT"
        <> value 3
        <> help "The number of rows in each block"
        <> showDefault
      ) :: Parser Int)
      <**> helper
    desc = progDesc "Generates a new sudoku board with MxN sectors"

solveCommand = command "solve" (info parser desc)
  where
    parser = solve 
      <$> argument str (
        metavar "FILE"
        <> help "The board containing the serialized board"
      )
      <**> helper
    desc = progDesc "Solves the given sudoku boards"

generate :: FilePath -> Int -> Int -> IO ()
-- generate = undefined 
generate file m n = withFile file WriteMode (\handle -> do
  gen <- getStdGen
  let {
    board = Generate.generate (m,n) 17 gen
  };
  putStrLn "Generated:\n"
  print board
  hPutStr handle (Board.serialize board))

solve :: FilePath -> IO ()
solve file = withFile file ReadMode (\handle -> do
  contents <- hGetContents handle
  case solve' contents of
    Nothing -> do putStrLn "No Solution"
    Just b -> do print b)
  
solve' :: String -> Maybe Board
solve' strbrd = case Solve.solve (Board.deserialize strbrd) of
  [] -> Nothing 
  b:bs -> Just b