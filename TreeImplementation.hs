module TreeImplementation where 

import Data.List
import Utils

-- Position Definition
type Positions = [Position]
data Position = Position (Int, Int) deriving (Eq)

instance Show Position where
    show position =  "(" ++ fst x ++ "," ++ snd y ++ ")"

data Point = Point  { position :: Position
                    , neighbour :: Positions
                    , value :: Char
                    } deriving (Eq)

instance Show Point where
    show (Point position _ value) = show position ++ ":" ++ show value

-- Board Definition
data Board = Board [[Point]]

instance Read Board where
    readsPrec _ = parseBoard

parseBoard :: String -> [(Board, String)]
parseBoard txtBoard = [(Board board, "")]
                    where
                        board = split 0 (lines txtBoard) [] 

split :: Int -> [String] -> [[Char]] -> [[Char]]
split y lines board
    | (y==(length lines)) = reverse board
    | otherwise = split (y+1) lines newBoard
    where 
        newBoard = (splitLine 0 y (lines!!y) []):board

splitLine :: Int -> Int -> String -> [Char] -> [Char]
splitLine x y line points
    | (x == (length line)) = reverse points
    | otherwise = splitLine (x+1) y line newPoints
    where
        newPoints = ((Point (Position x y) [] (line!!x))):points

getPoint :: Position -> Board -> Char
getPoint (Position x y) (Board board) 
    | (y >= (length board) || x >= (length (board!!0)) = Nothing
    | otherwise = ((board!!y)!!x)

-- constructTree :: [String] -> Position -> Positions -> Point
-- constructTree board init = Point 

-- constructPoint :: [String] -> Position -> Position -> Positions -> Point
-- constructPoint board prec position walls = Point position prec 
--                                     where 
--                                         neighbour = filter ( `notElem` walls ) [  Position x (y-1),  
--                                                                                   Position x (y+1), 
--                                                                                   Position (x-1) y, 
--                                                                                   Position (x+1) y ]
--                                         availableNeighbour = filter (/= prec) Position