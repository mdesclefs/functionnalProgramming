module Labyrinth where 

import System.Random
import Data.List
import Data.Maybe
import qualified Parser
import qualified Grammar
import qualified Utils


-- Position Definition
data Position = Position { x :: Int
                         , y :: Int
                         } deriving (Eq)

instance Show Position where
    show (Position x y) =  "(" ++ show x ++ "," ++ show y ++ ")"


-- Labyrinth Definition
data Labyrinth = Labyrinth  { tiles :: [[Tile]]
                            , extraTile :: Tile
                            }

instance Show Labyrinth where
    show (Labyrinth tiles extra) = printLabyrinth tiles extra

printLabyrinth :: [[Tile]] -> Tile -> String
printLabyrinth arr extra = display
    where   labyrinthPrinted = printLabyrinth' arr 0 (line++header)
            header = "--- 1 ---- 2 ---- 3 ---- 4 ---- 5 ---- 6 ---- 7 ---------\n"
            line =   "---------------------------------------------------------\n"
            display = labyrinthPrinted ++ (header++line) ++ "Extra tile : " ++ show extra ++ "\n"

printLabyrinth' :: [[Tile]] -> Int -> String -> String
printLabyrinth' labyrinthRaw y labyrinth
    | (y == size) = labyrinth
    | otherwise = printLabyrinth' labyrinthRaw (y+1) newLabyrinth
    where   line = (printLine (labyrinthRaw !! y) 0 beginHeader) ++ endHeader
            -- header = if odd y then (show (div (y+1) 2)++"  ") else ".  "
            header = show (y+1)
            beginHeader = header ++ " | "
            endHeader = " | " ++ header ++ "\n"
            newLabyrinth = labyrinth ++ line
            size = length labyrinthRaw

printLine :: [Tile] -> Int -> String -> String
printLine lineRaw x line
    | (x == size) = line
    | otherwise = printLine lineRaw (x+1) newLine
    where   newLine = line ++ (show (lineRaw !! x))
            size = length lineRaw

-- Initilization labyrinth

fixedLabyrinth :: [[Tile]]
fixedLabyrinth = 
            [
                [ 
                    Tile Grammar.East Grammar.Corner 0,
                    Tile Grammar.South Grammar.Tshape 0,
                    Tile Grammar.South Grammar.Tshape 0,
                    Tile Grammar.South Grammar.Corner 0
                ],
                [],
                [   
                    Tile Grammar.East Grammar.Tshape 0,
                    Tile Grammar.East Grammar.Tshape 0,
                    Tile Grammar.South Grammar.Tshape 0,
                    Tile Grammar.West Grammar.Tshape 0
                ],
                [],
                [   
                    Tile Grammar.East Grammar.Tshape 0,
                    Tile Grammar.North Grammar.Tshape 0,
                    Tile Grammar.West Grammar.Tshape 0,
                    Tile Grammar.West Grammar.Tshape 0
                ],
                [],
                [
                    Tile Grammar.North Grammar.Corner 0,
                    Tile Grammar.North Grammar.Tshape 0,
                    Tile Grammar.North Grammar.Tshape 0,
                    Tile Grammar.West Grammar.Corner 0
                ]
            ]

initLabyrinth :: [Float] -> Labyrinth
initLabyrinth randomList = Labyrinth { 
                              tiles = labyrinthWithTreasures 
                            , extraTile = remainingTile
                            }
    where   (labyrinth, remainingTile) = initLabyrinth' 0 (generateTilesList randomList) fixedLabyrinth
            labyrinthWithTreasures = distributeTreasures labyrinth treasures
            treasures = generateTreasure randomList


initLabyrinth' :: Int -> [Tile] -> [[Tile]] -> ([[Tile]], Tile)
initLabyrinth' y tilesList labyrinth
    | (y == 7) = (labyrinth, (tilesList!!0))
    | otherwise = initLabyrinth' (y+1) restTiles newLabyrinth
    where   newLabyrinth = Utils.edit y newLine labyrinth
            newLine = initLine y 1 tiles (labyrinth!!y)
            (tiles, restTiles) = splitAt tilesNbr tilesList
            tilesNbr = (if even y then 3 else 7)

initLine :: Int -> Int -> [Tile] -> [Tile] -> [Tile]
initLine y x [] line = line
initLine y x (newTile:restTiles) line
    | (x == 8) = line
    | (even y) = initLine y (x+2) restTiles newLine
    | otherwise = initLine y (x+1) restTiles newLine
    where   newLine = (Utils.insert newTile x line)

-- Labyrinth Action

putExtraTile :: Position -> Labyrinth -> [Player] -> (Labyrinth, [Player], Bool)
putExtraTile position labyrinth players
    | (elem position verticalAvailable && isJust verticalSlide) = (slideColumn position labyrinth, (fromMaybe players verticalSlide), True)
    | (elem position horizontalAvailable && isJust horizontalSlide) = (slideRow position labyrinth, (fromMaybe players horizontalSlide), True)
    | otherwise = (labyrinth, players, False)
    where   verticalAvailable = [(Position x y) | x <- [2,4,6], y <- [1,7]]
            horizontalAvailable = [(Position y x) | x <- [2,4,6], y <- [1,7]]
            verticalSlide = slidePlayers 1 players position
            horizontalSlide = slidePlayers 0 players position

slideRow :: Position -> Labyrinth -> Labyrinth
slideRow (Position x y) (Labyrinth tiles extraTile) = Labyrinth {tiles = tiles', extraTile = extraTile'}
    where   line = tiles !! (y-1)
            (firstTile:rest) = line
            newLine =   if x == 1 then 
                            Utils.delete (length line) (extraTile:line)
                        else
                            rest ++ [extraTile]
            extraTile' = if x == 1 then
                            last rest
                         else
                            firstTile
            tiles' = Utils.edit (y-1) newLine tiles


slideColumn :: Position -> Labyrinth -> Labyrinth
slideColumn (Position x y) (Labyrinth tiles extraTile) = Labyrinth {tiles = tiles', extraTile = extraTile'}
    where   (tiles', extraTile') = slideColumn' direction tiles extraTile (x-1) (y-1) 
            direction = if y == 1 then -- down
                            0
                        else
                            1

slideColumn' :: Int -> [[Tile]] -> Tile -> Int -> Int -> ([[Tile]], Tile)
slideColumn' 1 labyrinth tile _ (-1) = (labyrinth, tile)
slideColumn' 0 labyrinth tile _ 7 = (labyrinth, tile)
slideColumn' direction labyrinth tile x y = slideColumn' direction newLabyrinth tile' x nextY
    where   nextY =   if direction == 0 then -- down
                            y+1
                        else
                            y-1
            line = labyrinth !! y
            (tile', line') = Utils.editAndSave x tile line
            newLabyrinth = Utils.edit y line' labyrinth


-- Treasures Part

generateTreasure :: [Float] -> [Position]
generateTreasure random = generateTreasure' random allPositions []
    where allPositions = [(Position x y) | x <- [0..6], y <- [0..6], (x,y) /= (0, 0), (x,y) /= (6, 6), (x,y) /= (0, 6), (x,y) /= (6, 0)]

generateTreasure' :: [Float] -> [Position] -> [Position] -> [Position]
generateTreasure' (randomNotRanged:restRandom) allPositions listTreasure
    | (length listTreasure == 24) = listTreasure
    | otherwise = generateTreasure' restRandom restPosition newListTreasure
    where   randomIndex = Utils.getInRange randomNotRanged 0 (length allPositions)-1
            (position, restPosition) = Utils.getAndDelete randomIndex allPositions
            newListTreasure = position:listTreasure

distributeTreasures :: [[Tile]] -> [Position] -> [[Tile]]
distributeTreasures labyrinth [] = labyrinth
distributeTreasures labyrinth ((Position x y):restPosition) = distributeTreasures newLabyrinth restPosition
    where   newLabyrinth = Utils.edit y newLine labyrinth
            line = labyrinth!!y
            tile = line!!x
            newTile = tile { treasure = (24 - length restPosition) }
            newLine = Utils.edit x newTile line

-- Tile Representation

data Tile = Tile {  direction :: Grammar.Direction
                 ,  kinds :: Grammar.Kind
                 ,  treasure :: Int}

instance Show Tile where
    show (Tile direction kinds treasure)
        | (kinds == Grammar.Corner) = (showCorner direction) ++ showTreasure
        | (kinds == Grammar.Tshape) = (showTshaped direction) ++ showTreasure
        | (kinds == Grammar.Line) = (showLine direction) ++ showTreasure
        | otherwise = show 'X'
        where showTreasure = if treasure > 0 then 
                                (
                                    if treasure < 10 then " ( " ++ show treasure ++ ") " 
                                    else " (" ++ show treasure ++ ") " 
                                )
                             else "      "

generateTile :: Grammar.Kind -> Int -> Tile
generateTile kinds random = Tile{   direction = direction
                                ,   kinds = kinds
                                ,   treasure = 0
                                }
                where direction = directions!!random
                      directions = [Grammar.North ..]


generateTilesList :: [Float] -> [Tile]
generateTilesList randomList = Utils.mix (generateTilesList' 1 randomList []) randomList

generateTilesList' :: Int -> [Float] -> [Tile] -> [Tile]
generateTilesList' x (randomNotRanged:restRandom) tileList
    | (x == 36) = tileList
    | (x > 24) = generateTilesList' (x+1) restRandom ((generateTile Grammar.Line random):tileList)
    | (x > 16) = generateTilesList' (x+1) restRandom ((generateTile Grammar.Tshape random):tileList)
    | (x > 0) = generateTilesList' (x+1) restRandom ((generateTile Grammar.Corner random):tileList)
    | otherwise = []
    where random = Utils.getInRange randomNotRanged 0 (length [Grammar.North ..])-1

showCorner :: Grammar.Direction -> String
showCorner direction 
    | (direction == Grammar.North) = "╚"
    | (direction == Grammar.East) = "╔"
    | (direction == Grammar.South) = "╗"
    | (direction == Grammar.West) = "╝"
    | otherwise = "X"

showTshaped :: Grammar.Direction -> String
showTshaped direction 
    | (direction == Grammar.North) = "╩"
    | (direction == Grammar.East) = "╠"
    | (direction == Grammar.South) = "╦"
    | (direction == Grammar.West) = "╣"
    | otherwise = "X"

showLine :: Grammar.Direction -> String
showLine direction 
    | (direction == Grammar.North) = "║"
    | (direction == Grammar.East) = "═"
    | (direction == Grammar.South) = "║"
    | (direction == Grammar.West) = "═"
    | otherwise = "X"


data Player = Player { color :: Grammar.Color
                     , position :: Position
                     , cards :: [(Int, Bool)]
                     }

showCards :: [(Int, Bool)] -> String
showCards cards = showCards' cards ""

showCards' :: [(Int,Bool)] -> String -> String
showCards' [] line = line
showCards' ((treasure, state):restCards) line = showCards' restCards newLine
    where   newLine = line ++ (if treasure < 10 then " " else "") ++ show treasure ++ " : " ++ (if state then "v" else "x") ++ "\n"

instance Show Player where
    show (Player color position cards) = "Color : " ++ show color ++ "\nPosition : " ++ show position ++ "\n" ++ (showCards cards)

fixedPlayers :: [Player]
fixedPlayers =  [
                    Player Grammar.Red (Position 0 0) [],
                    Player Grammar.Green (Position 0 6) [],
                    Player Grammar.Blue (Position 6 0) [],
                    Player Grammar.Yellow (Position 6 6) []
                ]

initPlayers :: [Float] -> [Player]
initPlayers randomList = initPlayers' randomList [1..24] fixedPlayers []

initPlayers' :: [Float] -> [Int] -> [Player] -> [Player] -> [Player]
initPlayers' _ [] [] players = players 
initPlayers' randomList treasures (player:restPlayer) players = initPlayers' randomList restTreasures restPlayer newPlayers
    where   treasuresPerPlayer = 6
            (newPlayer, restTreasures) = assignTreasures randomList treasuresPerPlayer treasures player
            newPlayers = players ++ [newPlayer]

assignTreasures :: [Float] -> Int -> [Int] -> Player -> (Player, [Int])
assignTreasures _ 0 treasures player = (player, treasures)
assignTreasures (randomNotRanged:restRandom) treasuresLeft treasures player = assignTreasures restRandom (treasuresLeft-1) restTreasures newPlayer
    where   randomIndex = Utils.getInRange randomNotRanged 0 (length treasures)-1
            (treasure, restTreasures) = Utils.getAndDelete randomIndex treasures
            newPlayer = player { cards = (treasure,False):(cards player) }

slidePlayers :: Int -> [Player] -> Position -> Maybe [Player]
-- axis 1 vertical
-- axis 0 horizontal
slidePlayers axis players (Position x y) = slidePlayers' axis players axisValue []
    where axisValue =   if even y then -- fst 1 = y ; 0 = x
                            (x, y)
                        else
                            (y, x)

slidePlayers' :: Int -> [Player] -> (Int, Int) -> [Player] -> Maybe [Player]
slidePlayers' axis [] axisValue players = Just players
slidePlayers' axis (player:restPlayers) axisValue players 
    | (needToSwap' && outOfGame axis newX newY) = Nothing
    | otherwise = slidePlayers' axis restPlayers axisValue newPlayers
    where   newPlayers = [player'] ++ players
            playerPosition = position player
            newX = if (fst axisValue) == 1 then (x playerPosition)+1 else (x playerPosition)-1
            newY = if (fst axisValue) == 1 then (y playerPosition)+1 else (y playerPosition)-1
            newPosition = 
                if axis == 1 then 
                    playerPosition { y = newY }
                else 
                    playerPosition { x = newX }
            needToSwap' = needToSwap axis (snd axisValue) player
            player' = if needToSwap' then player { position = newPosition } else player

outOfGame :: Int -> Int -> Int -> Bool
outOfGame axis newX newY
    | (axis == 1 && newY < 0) = True
    | (axis == 1 && newY > 6) = True
    | (axis == 0 && newX < 0) = True
    | (axis == 0 && newX > 6) = True
    | otherwise = False  

needToSwap :: Int -> Int -> Player -> Bool
needToSwap axis axisValue (Player _ (Position x y) _) = if axis == 0 then axisValue == y else axisValue == x
