module Game where

import Data.List
import Data.Char
import qualified Labyrinth
import qualified Player 
import qualified Position
import qualified Tile
import qualified Utils

data Game = Game    { labyrinth :: Labyrinth.Labyrinth
                    , players :: [Player.Player]
                    , current_player :: Int
                    }


save :: Game -> String
save (Game labyrinth players current_player) = unwords (words (map toLower (playersTxt ++ " " ++ labyrinthTxt))) -- unwords words to remove multiple space
    where labyrinthTxt = Labyrinth.save labyrinth
          playersTxt = Player.save players current_player

instance Show Game where
    show (Game labyrinth players current_player) = show labyrinth ++ showOthersPlayersInfo otherPlayers ++ show (players !! current_player)
        where otherPlayers = Utils.delete current_player players

showOthersPlayersInfo :: [Player.Player] -> String
showOthersPlayersInfo [] = ""
showOthersPlayersInfo player = (showOthersPlayersInfo' player "Players informations:\nColor\t Position\t Cards Left\n") ++ "\n"

showOthersPlayersInfo' :: [Player.Player] -> String -> String
showOthersPlayersInfo' [] infos = infos
showOthersPlayersInfo' (player:players) infos = showOthersPlayersInfo' players newInfos
    where newInfos = infos ++ playerinfos
          playerinfos = show color ++ "\t " ++ show position ++ "\t\t " ++ show cardsLeft ++ "\n"
          color = Player.color player
          cardsLeft = length (Player.cards player)
          position = Player.position player

-- Core functionnality

initGame :: [Player.Control] -> [Float] -> Game
initGame controls randomList = Game labyrinth players 0
    where labyrinth = Labyrinth.initLabyrinth randomList
          players = Player.initPlayers controls randomList

putExtraTile :: Position.Position -> Tile.Direction -> Game -> (Game, Bool)
putExtraTile position direction (Game labyrinth players current_player) = ((Game newLabyrinth newPlayers current_player), result)
    where (newLabyrinth, newPlayers, result) = Labyrinth.putExtraTile position direction labyrinth players

putExtraTileAI :: [Float] -> Game -> (Game, [Float])
putExtraTileAI (randomPositionInd:(randomDirectionInd:restRandom)) (Game labyrinth players current_player) = ((Game newLabyrinth newPlayers current_player), restRandom)
    where (newLabyrinth, newPlayers, _) = Labyrinth.putExtraTile randomPosition randomDirection labyrinth players
          verticalAvailable = [(Position.Position x y) | x <- [2,4,6], y <- [1,7]]
          horizontalAvailable = [(Position.Position y x) | x <- [2,4,6], y <- [1,7]]
          availablePositions = verticalAvailable ++ horizontalAvailable
          randomPosition = availablePositions !! (Utils.getInRange randomPositionInd 0 (length availablePositions)-1)
          randomDirection = [Tile.North ..] !! (Utils.getInRange randomDirectionInd 0 3)

movePawn :: Position.Position -> Game -> (Game, Bool)
movePawn position (Game labyrinth players current_player) = ((Game labyrinth newPlayers current_player), result)
    where (newPlayer, result) = Labyrinth.movePawn cleanedPosition labyrinth (players !! current_player)
          newPlayers = Utils.edit current_player newPlayer players
          cleanedPosition = (Position.Position ((Position.x position)-1) ((Position.y position)-1))

movePawnAI :: [Float] -> Game -> (Game, [Float])
movePawnAI randomList (Game labyrinth players current_player) = ((Game labyrinth newPlayers current_player), restRandom)
    where player = (players !! current_player) 
          (randomPosition, restRandom)  = Player.findNextPosition player (Labyrinth.reachablePositions player labyrinth) randomList 
          (newPlayer, _) = Labyrinth.movePawn randomPosition labyrinth player
          newPlayers = Utils.edit current_player newPlayer players

gatherTreasures :: Game -> (Game, [Int])
gatherTreasures (Game labyrinth players current_player) = ((Game labyrinth newPlayers current_player), treasuresGathered)
    where player = (players !! current_player) 
          treasuresReachable = Labyrinth.gatherTreasures player labyrinth
          treasuresGathered = sort (intersect (Player.cards player) treasuresReachable)
          newPlayer = Player.gatherTreasures player treasuresReachable
          newPlayers = Utils.edit current_player newPlayer players

nextPlayer :: Game -> Game
nextPlayer game = game { current_player = nextPlayerInd }
    where playersNbr = (length (players game)) - 1
          nextPlayer = (current_player game) + 1
          nextPlayerInd = if (nextPlayer > playersNbr) then 0 else nextPlayer

getCurrentPlayer :: Game -> Player.Player
getCurrentPlayer (Game _ players current_player) = players !! current_player

isFinish :: Game -> (Bool, Player.Color)
isFinish (Game _ players _) = isFinish' players

isFinish' :: [Player.Player] -> (Bool, Player.Color)
isFinish' [] = (False, Player.None)
isFinish' (player:players)
    | Player.hasWin player = (True, Player.color player)
    | otherwise = isFinish' players
