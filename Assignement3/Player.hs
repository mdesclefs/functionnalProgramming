module Player where 

import Data.List
import Position
import qualified Utils
import Tile

data Color = None | Red | Green | Blue | Yellow  deriving (Eq, Enum, Read, Show)
data Control = Human | AI deriving (Eq, Enum, Read, Show)

data Player = Player { color :: Color
                     , position :: Position
                     , cards :: [Int]
                     , control :: Control
                     }

save :: [Player] -> Int -> String
save players current_player = save' players 0 current_player ""

save' :: [Player] -> Int -> Int -> String -> String
save' players done current playersTxt 
    | done == ((length players)) = playersTxt
    | otherwise = save' players (done+1) newCurrent newPlayersTxt
    where newCurrent = if length players == (current+1) then 0 else (current+1)
          newPlayersTxt = playersTxt ++ " " ++ savePlayer (players !! current)

savePlayer :: Player -> String
savePlayer (Player color (Position x y) cards control) = (show color) ++ " " ++ (show control) ++ " " ++ (show x) ++ " " ++ (show y) ++ " " ++ (saveCards cards "")

saveCards :: [Int] -> String -> String
saveCards [] cardsTxt = cardsTxt
saveCards (card:cards) cardsTxt = saveCards cards newCardsTxt
    where newCardsTxt = cardsTxt ++ " " ++ (show card)

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
                    Player Red (initialPosition' Red) [] Human,
                    Player Green (initialPosition' Green) [] Human,
                    Player Blue (initialPosition' Blue) [] Human,
                    Player Yellow (initialPosition' Yellow) [] Human
                ]

initialPosition :: Player -> Position
initialPosition (Player color _ _ _) = initialPosition' color

initialPosition' :: Color -> Position
initialPosition' color
    | color == Red = (Position 0 0)
    | color == Green = (Position 0 6)
    | color == Blue = (Position 6 0)
    | color == Yellow = (Position 6 6)
    | otherwise = (Position (-1) (-1))

initPlayers :: [Control] -> [Float] -> [Player]
initPlayers controls randomList = initPlayers' controls (div 24 (length controls)) randomList [1..24] fixedPlayers []

initPlayers' :: [Control] -> Int -> [Float] -> [Int] -> [Player] -> [Player] -> [Player]
initPlayers' [] _ _ _ _ players = players 
initPlayers' (control:restControls) treasuresPerPlayer randomList treasures (player:restPlayer) players = initPlayers' restControls treasuresPerPlayer randomList restTreasures restPlayer newPlayers
    where   (newPlayer, restTreasures) = assignTreasures randomList treasuresPerPlayer treasures player
            newPlayers = players ++ [newPlayer { control = control, cards = (sort (cards newPlayer))}]

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
            newX = if (fst axisValue) == 0 then (x playerPosition)+1 else (x playerPosition)-1
            newY = if (fst axisValue) == 0 then (y playerPosition)+1 else (y playerPosition)-1
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

hasWin :: Player -> Bool
hasWin (Player color position cards _) 
    | ((length cards) == 0 && position == (initialPosition' color)) = True
    | otherwise = False

isAI :: Player -> Bool
isAI (Player _ _ _ control) = (control == AI)

-- IA PART

findNextPosition :: Player -> [Position] -> [Float] -> (Position, [Float])
findNextPosition player [] randomList = (position player, randomList)
findNextPosition player reachablePositions (randomNotRanged:restRandom) 
    | ((length (cards player)) == 0) && (elem (initialPosition player) reachablePositions) = (initialPosition player, restRandom) -- Winning situation
    | ((length (cards player)) == 0) = ((findClosest (position player) reachablePositions randomNotRanged), restRandom)
    | otherwise = (newPosition, restRandom)
    where 
        randomIndex = Utils.getInRange randomNotRanged 0 (length reachablePositions)-1
        newPosition = reachablePositions !! randomIndex

findClosest :: Position -> [Position] -> Float -> Position
findClosest position reachablePositions randomNotRanged = randomClosestPosition
    where potentialPositions = findClosest' position reachablePositions 1
          randomIndex = Utils.getInRange randomNotRanged 0 (length potentialPositions)-1
          randomClosestPosition = potentialPositions !! randomIndex
    
findClosest' :: Position -> [Position] -> Int -> [Position]
findClosest' (Position x y) reachablePositions level
    | level > 4 = []
    | length closestPositions == 0 = findClosest' (Position x y) reachablePositions (level+1)
    | otherwise = closestPositions
    where closestPositions = [(Position (x+xMove) (y+yMove)) | xMove <- [(-level)..(level)], yMove <- [(-level)..(level)], (elem (Position (x+xMove) (y+yMove)) reachablePositions)]