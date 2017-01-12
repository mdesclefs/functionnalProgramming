module Labyrinth where 

import System.Random
import Data.List
import Data.Maybe

import qualified Utils
import Player
import Tile
import Position

-- Labyrinth Definition
data Labyrinth = Labyrinth  { tiles :: [[Tile]]
                            , extraTile :: Tile
                            }

instance Show Labyrinth where
    show (Labyrinth tiles extra) = printLabyrinth tiles extra

printLabyrinth :: [[Tile]] -> Tile -> String
printLabyrinth arr extra = display
    where   labyrinthPrinted = printLabyrinth' arr 0 (line++header)
            header = "----- 1 ---- 2 ---- 3 ---- 4 ---- 5 ---- 6 ---- 7 -----------\n"
            line =   "-------------X-------------X-------------X-------------------\n"
            display = "\n" ++ labyrinthPrinted ++ (header++line) ++ "Extra tile : " ++ show extra ++ "(" ++ show (direction extra) ++ ")\n\n"

printLabyrinth' :: [[Tile]] -> Int -> String -> String
printLabyrinth' labyrinthRaw y labyrinth
    | (y == size) = labyrinth
    | otherwise = printLabyrinth' labyrinthRaw (y+1) newLabyrinth
    where   line = (printLine (labyrinthRaw !! y) 0 beginHeader) ++ endHeader
            -- header = if odd y then (show (div (y+1) 2)++"  ") else ".  "
            header = show (y+1)
            beginHeader = (if even (y+1) then "X " else "| ") ++ header ++ " | "
            endHeader = " | " ++ header ++ (if even (y+1) then " X\n" else " |\n")
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
                    Tile East Corner 0,
                    Tile South Tshape 0,
                    Tile South Tshape 0,
                    Tile South Corner 0
                ],
                [],
                [   
                    Tile East Tshape 0,
                    Tile East Tshape 0,
                    Tile South Tshape 0,
                    Tile West Tshape 0
                ],
                [],
                [   
                    Tile East Tshape 0,
                    Tile North Tshape 0,
                    Tile West Tshape 0,
                    Tile West Tshape 0
                ],
                [],
                [
                    Tile North Corner 0,
                    Tile North Tshape 0,
                    Tile North Tshape 0,
                    Tile West Corner 0
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

-- On Labyrinth Action

putExtraTile :: Position -> Direction -> Labyrinth -> [Player] -> (Labyrinth, [Player], Bool)
putExtraTile position direction labyrinth players
    | (elem position verticalAvailable && isJust verticalSlide) = (slideColumn position direction labyrinth, (fromMaybe players verticalSlide), True)
    | (elem position horizontalAvailable && isJust horizontalSlide) = (slideRow position direction labyrinth, (fromMaybe players horizontalSlide), True)
    | otherwise = (labyrinth, players, False)
    where   verticalAvailable = [(Position x y) | x <- [2,4,6], y <- [1,7]]
            horizontalAvailable = [(Position y x) | x <- [2,4,6], y <- [1,7]]
            verticalSlide = slidePlayers 1 players position
            horizontalSlide = slidePlayers 0 players position

slideRow :: Position -> Direction -> Labyrinth -> Labyrinth
slideRow (Position x y) tileDirection (Labyrinth tiles extraTile) = Labyrinth {tiles = tiles', extraTile = extraTile'}
    where   line = tiles !! (y-1)
            (firstTile:rest) = line
            extraTileUpdated = extraTile {direction = tileDirection}
            newLine =   if x == 1 then 
                            Utils.delete (length line) ((extraTileUpdated):line)
                        else
                            rest ++ [extraTileUpdated]
            extraTile' = if x == 1 then
                            last rest
                         else
                            firstTile
            tiles' = Utils.edit (y-1) newLine tiles


slideColumn :: Position -> Direction -> Labyrinth -> Labyrinth
slideColumn (Position x y) tileDirection (Labyrinth tiles extraTile) = Labyrinth {tiles = tiles', extraTile = extraTile'}
    where   (tiles', extraTile') = slideColumn' direction tiles (extraTile {direction = tileDirection}) (x-1) (y-1) 
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

-- On Player Action

-- movePawn :: Position -> Game -> (Game, Bool)
-- movePawn position game = Labyrinth.movePawn position game

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