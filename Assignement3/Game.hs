module Game where

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

-- movePawn :: Position.Position -> Game -> (Game, Bool)
-- movePawn position (Game labyrinth players current_player) = Labyrinth.movePawn position labyrinth (players !! current_player)
