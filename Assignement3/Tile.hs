module Tile where 

import qualified Utils

-- Tile Representation
data Kind = Corner | Tshape | Line deriving (Eq, Enum, Read, Show)
data Direction = None | North | East | South | West deriving (Eq, Enum, Read, Show)

data Tile = Tile {  direction :: Direction
                 ,  kinds :: Kind
                 ,  treasure :: Int}

instance Show Tile where
    show (Tile direction kinds treasure)
        | (kinds == Corner) = (showCorner direction) ++ showTreasure
        | (kinds == Tshape) = (showTshaped direction) ++ showTreasure
        | (kinds == Line) = (showLine direction) ++ showTreasure
        | otherwise = show 'X'
        where showTreasure = if treasure > 0 then 
                                (
                                    if treasure < 10 then " ( " ++ show treasure ++ ") " 
                                    else " (" ++ show treasure ++ ") " 
                                )
                             else "      "

generateTile :: Kind -> Int -> Tile
generateTile kinds random = Tile{   direction = direction
                                ,   kinds = kinds
                                ,   treasure = 0
                                }
                where direction = directions!!random
                      directions = [North ..]


generateTilesList :: [Float] -> [Tile]
generateTilesList randomList = Utils.mix (generateTilesList' 1 randomList []) randomList

generateTilesList' :: Int -> [Float] -> [Tile] -> [Tile]
generateTilesList' x (randomNotRanged:restRandom) tileList
    | (x == 36) = tileList
    | (x > 24) = generateTilesList' (x+1) restRandom ((generateTile Line random):tileList)
    | (x > 16) = generateTilesList' (x+1) restRandom ((generateTile Tshape random):tileList)
    | (x > 0) = generateTilesList' (x+1) restRandom ((generateTile Corner random):tileList)
    | otherwise = []
    where random = Utils.getInRange randomNotRanged 0 (length [North ..])-1

showCorner :: Direction -> String
showCorner direction 
    | (direction == North) = "╚"
    | (direction == East) = "╔"
    | (direction == South) = "╗"
    | (direction == West) = "╝"
    | otherwise = "X"

showTshaped :: Direction -> String
showTshaped direction 
    | (direction == North) = "╩"
    | (direction == East) = "╠"
    | (direction == South) = "╦"
    | (direction == West) = "╣"
    | otherwise = "X"

showLine :: Direction -> String
showLine direction 
    | (direction == North) = "║"
    | (direction == East) = "═"
    | (direction == South) = "║"
    | (direction == West) = "═"
    | otherwise = "X"