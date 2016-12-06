{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified System.Environment

import qualified Utils
import Data.List
import Data.Maybe

main :: IO ()
main = do [path] <- System.Environment.getArgs
          maze <- readFile path
          -- putStr $ show $ entrance $ read (maze :: Utils.Board)
          putStr $ show $ entrance $ (read maze::Utils.Board)


class (Read board, Show position) => Maze board position where
    entrance :: board -> position
    exits :: board -> [position]
    neighbourghs :: board -> position -> [position]
    shortest :: board -> Maybe [position]

instance Maze Utils.Board Utils.Position where
    entrance board = Utils.Position 3 2 ---(entrance board)
    exits board = [(Utils.Position 3 2)]
    neighbourghs board (Utils.Position x y) = [ Utils.Position x (y-1),  
                                            Utils.Position x (y+1), 
                                            Utils.Position (x-1) y, 
                                            Utils.Position (x+1) y ]
    shortest board = Just [(Utils.Position 3 2)]

instance Maze Utils.Board String where
    entrance board = show (Utils.Position 3 2)