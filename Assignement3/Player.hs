module Player where 

import Data.List
import Position
import qualified Utils
import Tile

data Color = Yellow | Red | Blue | Green deriving (Eq, Enum, Read, Show)
data Control = Human | AI deriving (Eq, Enum, Read, Show)

data Player = Player { color :: Color
                     , position :: Position
                     , cards :: [Int]
                     , control :: Control
                     }

showCards :: [Int] -> String
showCards cards = showCards' cards ""

showCards' :: [Int] -> String -> String
showCards' [] line = line
showCards' (treasure:restCards) line = showCards' restCards newLine
    where   newLine = line ++ show treasure ++ "\n"

instance Show Player where
    show (Player color position cards control) = "Personnal informations:\nColor : " ++ show color ++ "\nPosition : " ++ show position ++ "\nCards : " ++ (show cards) ++ "\n"

fixedPlayers :: [Player]
fixedPlayers =  [
                    Player Red (Position 0 0) [] Human,
                    Player Green (Position 0 6) [] Human,
                    Player Blue (Position 6 0) [] Human,
                    Player Yellow (Position 6 6) [] Human
                ]

initPlayers :: [Float] -> [Player]
initPlayers randomList = initPlayers' randomList [1..24] fixedPlayers []

initPlayers' :: [Float] -> [Int] -> [Player] -> [Player] -> [Player]
initPlayers' _ [] [] players = players 
initPlayers' randomList treasures (player:restPlayer) players = initPlayers' randomList restTreasures restPlayer newPlayers
    where   treasuresPerPlayer = 6
            (newPlayer, restTreasures) = assignTreasures randomList treasuresPerPlayer treasures player
            newPlayers = players ++ [newPlayer { cards = (sort (cards newPlayer))}]

assignTreasures :: [Float] -> Int -> [Int] -> Player -> (Player, [Int])
assignTreasures _ 0 treasures player = (player, treasures)
assignTreasures (randomNotRanged:restRandom) treasuresLeft treasures player = assignTreasures restRandom (treasuresLeft-1) restTreasures newPlayer
    where   randomIndex = Utils.getInRange randomNotRanged 0 (length treasures)-1
            (treasure, restTreasures) = Utils.getAndDelete randomIndex treasures
            newPlayer = player { cards = (treasure):(cards player) }

slidePlayers :: Int -> [Player] -> Position -> Maybe [Player]
-- axis 1 vertical
-- axis 0 horizontal
slidePlayers axis players (Position x y) = slidePlayers' axis players axisValue []
    where axisValue =   if even y then -- fst 1 = y ; 0 = x
                            (x-1, y-1)
                        else
                            (y-1, x-1)

slidePlayers' :: Int -> [Player] -> (Int, Int) -> [Player] -> Maybe [Player]
slidePlayers' axis [] axisValue players = Just players
slidePlayers' axis (player:restPlayers) axisValue players 
    | (needToSwap' && outOfGame axis newX newY) = Nothing
    | otherwise = slidePlayers' axis restPlayers axisValue newPlayers
    where   newPlayers = players ++ [player']
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
needToSwap axis axisValue (Player _ (Position x y) _ _) = if axis == 0 then axisValue == y else axisValue == x

movePawn :: Player -> Position -> Player
movePawn player (Position x y) = player { position = (Position (x) (y)) }

gatherTreasures :: Player -> [Int] -> Player
gatherTreasures player treasures = player { cards = ((cards player) \\ treasures) }

