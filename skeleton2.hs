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
          putStr $ show $ (exits $ (read maze::Utils.Board)::Utils.Positions)


class (Read board, Show position) => Maze board position where
    entrance :: board -> position
    exits :: board -> [position]
    neighbourghs :: board -> position -> [position]
    shortest :: board -> Maybe [position]

instance Maze Utils.Board Utils.Position where
    entrance (Utils.Board _ _ entrance _ _) = entrance
    exits (Utils.Board _ _ _ exits _) =  exits
    neighbourghs board (Utils.Position x y) = [ Utils.Position x (y-1),  
                                            Utils.Position x (y+1), 
                                            Utils.Position (x-1) y, 
                                            Utils.Position (x+1) y ]
    shortest board = Just [(Utils.Position 3 2)]
