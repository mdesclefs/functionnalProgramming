module Utils 
(
    Positions,
    Position(..),
    Board(..),
    parseBoard
) where 

import Data.List

-- Position Definition
type Positions = [Position]
data Position = Position { x :: Int
                         , y :: Int
                         } deriving (Eq)

instance Show Position where
    show position =  "(" ++ show (x position) ++ "," ++ show (y position) ++ ")"

-- Board Definition

-- data Board = Board [[Char]] deriving (Read) 
type Matrix = [[Char]]
data Board = Board  { width :: Int
                    , height :: Int
                    , entrance :: Position --Maybe if findEntrance
                    , exits :: [Position]
                    , walls :: [Position]
                    } deriving (Show)

instance Read Board where
    readsPrec _ = parseBoard

parseBoard :: String -> [(Board, String)]
parseBoard txtBoard = [(Board width height entrance exits walls, "")]
                    where
                            board = lines txtBoard
                            width = length (board !! 0) + 1
                            height = length (board) + 1
                            -- entrance = findEntrance board 0
                            entrance = (findChar '*' board 0 []) !! 0
                            exits = findChar '@' board 0 []
                            walls = findChar 'X' board 0 []


findEntrance :: Matrix -> Int -> Maybe Position
findEntrance board y 
    | (length board <= y) = Nothing
    | (findElem '*' (board !! y) /= -1) = Just (Position (findElem '*' (board !! y)) y)
    | otherwise = findEntrance board (y+1)

findElem :: Char -> [Char] -> Int
findElem pattern line = case elemIndex pattern line of
    Nothing -> -1
    Just index -> index

findChar :: Char -> Matrix -> Int -> [Position] -> [Position]
findChar pattern board y positions
    | (length board <= y) = positions
    | otherwise = findChar pattern board (y+1) (positions++(map (\x -> (Position x y)) (elemIndices pattern (board !! y))))