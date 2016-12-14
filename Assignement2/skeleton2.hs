{-# LANGUAGE MultiParamTypeClasses #-}

import qualified System.Environment

import qualified ObjectImplementation
import qualified ListImplementation
import Data.List
import Data.Maybe

main :: IO ()
main = do [path] <- System.Environment.getArgs
          maze <- readFile path
          putStr $ "Object Implementation : " 
          putStrLn $ show $ (shortest $ (read maze::ObjectImplementation.Board)::Maybe ObjectImplementation.Positions)
          putStr $ "List Implementation : "
          putStrLn $ show $ (shortest $ (read maze::ListImplementation.Board)::Maybe ListImplementation.Positions)

class (Read board, Show position) => Maze board position where
    entrance :: board -> position
    exits :: board -> [position]
    neighbourghs :: board -> position -> [position]
    shortest :: board -> Maybe [position]
    shortest' :: board -> [(position, position)] -> [(position,position)] -> Maybe [(position,position)]
    computePath :: board -> (position, position) -> [(position, position)] -> [position] -> [position]

-- First implementation

instance Maze ObjectImplementation.Board ObjectImplementation.Position where
    entrance (ObjectImplementation.Board _ _ entrance _ _) = entrance
    exits (ObjectImplementation.Board _ _ _ exits _) =  exits
    neighbourghs (ObjectImplementation.Board _ _ _ _ walls) (ObjectImplementation.Position x y) = filter ( `notElem` walls ) [  ObjectImplementation.Position x (y-1),  
                                                                                                                            ObjectImplementation.Position x (y+1), 
                                                                                                                            ObjectImplementation.Position (x-1) y, 
                                                                                                                            ObjectImplementation.Position (x+1) y ]
    shortest board 
        | (resultPath == Nothing) = Nothing
        | otherwise = Just (computePath board initPosition (fromJust resultPath) [fst initPosition])
         where 
            initQueue = [(entrance board, entrance board)]
            resultPath = (shortest' board initQueue [])
            initPosition = (fromJust resultPath) !! 0

    -- Apply a breathfirst algorithm using list as queue and a list of edge in order to retrieve the right path once an exist is found.
    shortest' board [] path = Nothing
    shortest' board (x:xs) path 
        | (actualPosition) `elem` (exits board) = Just newPath
        | otherwise = shortest' board newQueue newPath
        where
            actualPosition = snd x
            neighbourghsTupple = (concatMap (\next -> [(actualPosition,next)]) (neighbourghs board actualPosition))
            alreadyVisit = map snd path
            newQueue = xs++filter ((`notElem` alreadyVisit).snd) neighbourghsTupple
            newPath = x:path

    computePath board position tupplePath path
        | ((fst position) == (entrance board)) = drop 1 path -- Drop to remove init point 
        | otherwise = computePath board nextPositionTupple newTupplePath newPath
        where 
            nextPositionTupple = (filter ((==fst position).snd) tupplePath) !! 0
            nextPosition = fromJust (elemIndex nextPositionTupple (tupplePath))
            newTupplePath = drop nextPosition tupplePath
            newPath = (fst nextPositionTupple):path

-- Second implementation

instance Maze ListImplementation.Board ListImplementation.Position where
    entrance board = (ListImplementation.findInBoard board '*') !! 0
    exits board = ListImplementation.findInBoard board '@'
    neighbourghs board (ListImplementation.Position (x,y)) = filter ( `notElem` walls ) [   ListImplementation.Position (x, (y-1)),  
                                                                                            ListImplementation.Position (x, (y+1)), 
                                                                                            ListImplementation.Position ((x-1), y), 
                                                                                            ListImplementation.Position ((x+1), y) ]
                                                            where
                                                                walls = ListImplementation.findInBoard board 'X'

    shortest board 
        | (resultPath == Nothing) = Nothing
        | otherwise = Just (computePath board initPosition (fromJust resultPath) [fst initPosition])
         where 
            initQueue = [(entrance board, entrance board)]
            resultPath = (shortest' board initQueue [])
            initPosition = (fromJust resultPath) !! 0

    shortest' board [] path = Nothing
    shortest' board (x:xs) path 
        | (actualPosition) `elem` (exits board) = Just newPath
        | otherwise = shortest' board newQueue newPath
        where
            actualPosition = snd x
            neighbourghsTupple = (concatMap (\next -> [(actualPosition,next)]) (neighbourghs board actualPosition))
            alreadyVisit = map snd path
            newQueue = xs++filter ((`notElem` alreadyVisit).snd) neighbourghsTupple
            newPath = x:path

    computePath board position tupplePath path
        | ((fst position) == (entrance board)) = drop 1 path -- Drop to remove init point 
        | otherwise = computePath board nextPositionTupple newTupplePath newPath
        where 
            nextPositionTupple = (filter ((==fst position).snd) tupplePath) !! 0
            nextPosition = fromJust (elemIndex nextPositionTupple (tupplePath))
            newTupplePath = drop nextPosition tupplePath
            newPath = (fst nextPositionTupple):path      