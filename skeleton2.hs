{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified System.Environment

import qualified Utils
import Data.List
import Data.Maybe
-- MAYBE ON SHORTEST
main :: IO ()
main = do [path] <- System.Environment.getArgs
          maze <- readFile path
          -- putStr $ show $ entrance $ read (maze :: Utils.Board)
          putStr $ show $ (shortest $ (read maze::Utils.Board):: Utils.Positions)




class (Read board, Show position) => Maze board position where
    entrance :: board -> position
    exits :: board -> [position]
    neighbourghs :: board -> position -> [position]
    shortest :: board -> [position]
    shortest' :: board -> [(position, position)] -> [(position,position)] -> [(position,position)]
    computePath :: board -> (position, position) -> [(position, position)] -> [position] -> [position]

instance Maze Utils.Board Utils.Position where
    entrance (Utils.Board _ _ entrance _ _) = entrance
    exits (Utils.Board _ _ _ exits _) =  exits
    neighbourghs (Utils.Board _ _ _ _ walls) (Utils.Position x y) = filter ( `notElem` walls ) [   Utils.Position x (y-1),  
                                                                                                      Utils.Position x (y+1), 
                                                                                                      Utils.Position (x-1) y, 
                                                                                                      Utils.Position (x+1) y ]
    shortest board = computePath board initPosition (reverse (shortest' board initQueue initPath)) []
                     where 
                        initQueue = [(entrance board, entrance board)]
                        initPosition = initQueue !! 0
                        initPath = []

    shortest' board (x:xs) path 
        | (actualPosition) `elem` (exits board) = newPath
        | length (x:xs) == 0 = [] 
        | otherwise = shortest' board newQueue newPath
        where
            -- newQueue = xs++(snd x, (neighbourghs board x))
            actualPosition = snd x
            neighbourghsTupple = (concatMap (\next -> [(actualPosition,next)]) (neighbourghs board actualPosition))
            alreadyVisit = map snd path
            newQueue = xs++filter ((`notElem` alreadyVisit).snd) neighbourghsTupple
            newPath = x:path

    computePath board position tupplePath path
        | ((fst position) == (entrance board)) = reverse (path)
        | otherwise = computePath board nextPositionTupple newTupplePath newPath
        where 
            nextPositionTupple = (filter ((==fst position).snd) tupplePath) !! 0
            nextPosition = fromJust (elemIndex nextPositionTupple (tupplePath))
            newTupplePath = drop nextPosition tupplePath
            newPath = (fst nextPositionTupple):path