module ListImplementation where 

import Data.List
import Utils

-- Position Definition
type Positions = [Position]
data Position = Position (Int, Int) deriving (Eq)

instance Show Position where
    show (Position (x,y)) =  "(" ++ show x ++ "," ++ show y ++ ")"

-- Board Definition
data Board = Board [[Char]]

getHeight :: Board -> Int
getHeight (Board board) = (length board)

getWidth :: Board -> Int
getWidth (Board board) = (length (board !! 0))

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
        newPoints = (line!!x):points

getPoint :: Board -> Position -> Maybe Char
getPoint (Board board) (Position (x,y)) 
    | (y >= (length board)) = Nothing
    | (x >= (length (board !! 0))) = Nothing
    | otherwise = Just ( (board !! y) !! x)

getLine :: Board -> Int -> Maybe [Char]
getLine (Board board) y 
    | (y >= (length board)) = Nothing
    | otherwise = Just (board !! y)

findInBoard :: Board -> Char -> Positions
findInBoard board char = findInBoard' board char 0 []

findInBoard' :: Board -> Char -> Int -> Positions -> Positions
findInBoard' (Board board) pattern y result
    | (y==(length board)) = result
    | otherwise = findInBoard' (Board board) pattern (y+1) newResult
    where
        newResult = result ++ (map (\x -> (Position (x,y))) (elemIndices (pattern) (board!!y)))
