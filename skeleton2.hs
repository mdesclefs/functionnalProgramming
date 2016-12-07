{-# LANGUAGE MultiParamTypeClasses #-}

import qualified System.Environment

import qualified ListImplementation
import qualified TreeImplementation
import Data.List
import Data.Maybe
-- MAYBE ON SHORTEST

main :: IO ()
main = do [path] <- System.Environment.getArgs
          maze <- readFile path
          -- putStr $ show $ entrance $ read (maze :: ListImplementation.Board)
          putStr $ show $ (shortest $ (read maze::ListImplementation.Board):: ListImplementation.Positions)
          -- putStr $ show $ (showSolution $ (read maze::ListImplementation.Board) ([]::ListImplementation.Positions))

class (Read board, Show position) => Maze board position where
    entrance :: board -> position
    exits :: board -> [position]
    neighbourghs :: board -> position -> [position]
    shortest :: board -> [position]
    shortest' :: board -> [(position, position)] -> [(position,position)] -> [(position,position)]
    computePath :: board -> (position, position) -> [(position, position)] -> [position] -> [position]
    -- longest :: board -> [position]
    -- longest' :: board -> [[position]]
    showSolution :: board -> [position] -> String


instance Maze ListImplementation.Board ListImplementation.Position where
    entrance (ListImplementation.Board _ _ entrance _ _) = entrance
    exits (ListImplementation.Board _ _ _ exits _) =  exits
    neighbourghs (ListImplementation.Board _ _ _ _ walls) (ListImplementation.Position x y) = filter ( `notElem` walls ) [  ListImplementation.Position x (y-1),  
                                                                                                                            ListImplementation.Position x (y+1), 
                                                                                                                            ListImplementation.Position (x-1) y, 
                                                                                                                            ListImplementation.Position (x+1) y ]
    shortest board = computePath board initPosition resultPath []
                     where 
                        initQueue = [(entrance board, entrance board)]
                        initPath = []
                        resultPath = (shortest' board initQueue initPath)
                        initPosition = resultPath !! 0

    shortest' board (x:xs) path 
        | (actualPosition) `elem` (exits board) = newPath
        | (1+length xs) == 0 = [] 
        | otherwise = shortest' board newQueue newPath
        where
            actualPosition = snd x
            neighbourghsTupple = (concatMap (\next -> [(actualPosition,next)]) (neighbourghs board actualPosition))
            alreadyVisit = map snd path
            newQueue = xs++filter ((`notElem` alreadyVisit).snd) neighbourghsTupple
            newPath = x:path

    computePath board position tupplePath path
        | ((fst position) == (entrance board)) = path
        | otherwise = computePath board nextPositionTupple newTupplePath newPath
        where 
            nextPositionTupple = (filter ((==fst position).snd) tupplePath) !! 0
            nextPosition = fromJust (elemIndex nextPositionTupple (tupplePath))
            newTupplePath = drop nextPosition tupplePath
            newPath = (fst nextPositionTupple):path

    -- longest board = longest' board initPathList

    -- longest' board pathList
    --     | (actualPosition) `elem` (exits board) = pathList

    
    showSolution board path = do
        (y) <- [1..(ListImplementation.height board)]
        (x) <- [1..(ListImplementation.width board)]
        return $ ListImplementation.getComposant board (ListImplementation.Position x y)

instance Maze TreeImplementation.Board TreeImplementation.Position where
    entrance board = (TreeImplementation.findInBoard board "*") !! 0
    exits board = TreeImplementation.findInBoard board "@"
    neighbourghs board (Position (x,y)) = filter ( `notElem` walls ) [  TreeImplementation.Position (x, (y-1)),  
                                                                        TreeImplementation.Position (x, (y+1)), 
                                                                        TreeImplementation.Position ((x-1), y), 
                                                                        TreeImplementation.Position ((x+1), y) ]
                                        where
                                            walls = TreeImplementation.findInBoard board "X"