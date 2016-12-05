{-# LANGUAGE MultiParamTypeClasses #-}

import qualified System.Environment

import Utils
import Data.List
import Data.Maybe

main :: IO ()
main = do [path] <- System.Environment.getArgs
          maze <- readFile path
          putStr $ show $ shortest $ read maze


class (Read board, Show position) => Maze board position where
  entrance :: board -> position
  exits :: board -> [position]
  neighbourghs :: board -> position -> [position]
  shortest :: board -> Maybe [position]

instance Maze Board Position where
    entrance board = entrance board
    exits board = exits board
    shortest board = [(Position 3 2)]
    neighbourghs board (Position x y) = [Position x (y-1),  Position x (y+1), Position (x-1) y, Position (x+1) y]