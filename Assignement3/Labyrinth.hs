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

save :: Labyrinth -> String
save (Labyrinth tiles extraTile) = (Tile.saveExtraTile extraTile) ++ " " ++ (Tile.saveTiles tiles)

instance Show Labyrinth where
    show (Labyrinth tiles extra) = printLabyrinth tiles extra

printLabyrinth :: [[Tile]] -> Tile -> String
printLabyrinth arr extra = display
    where   labyrinthPrinted = printLabyrinth' arr 0 (line++header)
            header = "----- 1 ---- 2 ---- 3 ---- 4 ---- 5 ---- 6 ---- 7 -----------\n"
            line =   "-------------X-------------X-------------X-------------------\n"
            display = "\n" ++ labyrinthPrinted ++ (header++line) ++ "Extra tile : " ++ show extra ++ "\n\n"

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

loadLabyrinth :: [Tile] -> Tile -> Labyrinth
loadLabyrinth tiles xtile = Labyrinth (convertTiles tiles) xtile

convertTiles :: [Tile] -> [[Tile]]
convertTiles tiles = convertTiles' tiles []

convertTiles' :: [Tile] -> [[Tile]] -> [[Tile]]
convertTiles' [] matrix = matrix
convertTiles' tiles matrix = convertTiles' newTiles newMatrix
    where line = take 7 tiles
          newTiles = drop 7 tiles
          newMatrix = matrix ++ [line]

initLabyrinth :: [Float] -> Labyrinth
initLabyrinth randomList = Labyrinth { 
                              tiles = labyrinthWithTreasures 
                            , extraTile = remainingTile {direction = Tile.None}
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
slideRow (Position x y) tileDirection (Labyrinth tiles extraTile) = Labyrinth {tiles = tiles', extraTile = extraTile' {direction = Tile.None}}
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
slideColumn (Position x y) tileDirection (Labyrinth tiles extraTile) = Labyrinth {tiles = tiles', extraTile = extraTile' {direction = Tile.None}}
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

movePawn :: Position -> Labyrinth -> Player -> (Player, Bool)
movePawn position labyrinth player
    -- | ((Player.position player) == position) = (player, False) -- Player can not stand on same position
    | elem position reachablePositionsList = ((Player.movePawn player position), True)
    | otherwise = (player, False)
    where reachablePositionsList = reachablePositions player labyrinth

reachablePositions :: Player -> Labyrinth -> [Position]
reachablePositions (Player _ position _ _) labyrinth = reachablePositions' position position labyrinth []

reachablePositions' :: Position -> Position -> Labyrinth -> [Position] -> [Position]
reachablePositions' oldPosition newPosition labyrinth positionsList
    | not (y >= 0 && y < 7 && x >= 0 && x < 7) = positionsList
    | not (isReachable oldPosition newPosition labyrinth) = positionsList
    | elem newPosition positionsList = positionsList -- Already visited
    | otherwise = fromEastReachablePositions
    where (Position x y) = newPosition
          fromNorthReachablePositions = (reachablePositions' newPosition (Position x (y-1)) labyrinth (newPosition:positionsList))
          fromSouthReachablePositions = (reachablePositions' newPosition (Position x (y+1)) labyrinth (fromNorthReachablePositions))
          fromWestReachablePositions = (reachablePositions' newPosition (Position (x-1) y) labyrinth (fromSouthReachablePositions))
          fromEastReachablePositions = (reachablePositions' newPosition (Position (x+1) y) labyrinth (fromWestReachablePositions))

isReachable :: Position -> Position -> Labyrinth -> Bool
isReachable (Position fromX fromY) (Position toX toY) (Labyrinth tiles _) 
    | ((fromX == toX) && (fromY == toY)) = True
    | otherwise = hasConnections tilesList connections
    where fromTile = ((tiles !! fromY) !! fromX)
          toTile = ((tiles !! toY) !! toX)
          tilesList = fromTile:[toTile]
          connections = connectionsNeeded fromX fromY toX toY

connectionsNeeded :: Int -> Int -> Int -> Int -> [Direction]
connectionsNeeded fromX fromY toX toY 
    | (toX - fromX) == 1 = [East, West]
    | (toX - fromX) == -1 = [West, East]
    | (toY - fromY) == 1 = [South, North]
    | (toY - fromY) == -1 = [North, South]

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

gatherTreasures :: Player -> Labyrinth -> [Int]
gatherTreasures player labyrinth = gatherTreasures' labyrinth positions []
    where positions = (reachablePositions player labyrinth)

gatherTreasures' :: Labyrinth -> [Position] -> [Int] -> [Int]
gatherTreasures' labyrinth [] cards = cards
gatherTreasures' labyrinth ((Position x y):positions) cards = gatherTreasures' labyrinth positions newCards
    where tile = ((tiles labyrinth) !! y) !! x
          tileTreasure = treasure tile
          newCards = if tileTreasure > 0 then (tileTreasure:cards) else cards
