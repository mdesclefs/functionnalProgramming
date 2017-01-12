module Game where

import Data.List
import qualified Labyrinth
import qualified Player 
import qualified Position
import qualified Tile
import qualified Utils

data Game = Game    { labyrinth :: Labyrinth.Labyrinth
                    , players :: [Player.Player]
                    , current_player :: Int
                    }

instance Show Game where
    show (Game labyrinth players current_player) = show labyrinth ++ showOthersPlayersInfo otherPlayers ++ show (players !! current_player)
        where otherPlayers = Utils.delete current_player players

showOthersPlayersInfo :: [Player.Player] -> String
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

initGame :: [Float] -> Game
initGame randomList = Game labyrinth players 0
    where labyrinth = Labyrinth.initLabyrinth randomList
          players = Player.initPlayers randomList

putExtraTile :: Position.Position -> Tile.Direction -> Game -> (Game, Bool)
putExtraTile position direction (Game labyrinth players current_player) = ((Game newLabyrinth newPlayers current_player), result)
    where (newLabyrinth, newPlayers, result) = Labyrinth.putExtraTile position direction labyrinth players

movePawn :: Position.Position -> Game -> (Game, Bool)
movePawn position (Game labyrinth players current_player) = ((Game labyrinth newPlayers current_player), result)
    where (newPlayer, result) = Labyrinth.movePawn cleanedPosition labyrinth (players !! current_player)
          newPlayers = Utils.edit current_player newPlayer players
          cleanedPosition = (Position.Position ((Position.x position)-1) ((Position.y position)-1))

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

