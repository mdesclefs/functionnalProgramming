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
    shortest board = (Position 3 2):[]
    neighbourghs board position = let north = position {y=(y position)-1}
                                      south = position {y=(y position)+1}
                                      west = position {x=(x position)-1}
                                      east = position {x=(x position)+1}
                                  in  [north, south, west, east]