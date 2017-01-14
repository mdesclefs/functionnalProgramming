module Grammar where 


import Data.Char

import Parser

import qualified Game
import qualified Labyrinth
import qualified Player
import qualified Tile
import qualified Position
import qualified Utils

labyrinth :: Parser Game.Game
labyrinth = do  players <- players
                xtile <- xtile
                tiles <- tiles
                return $ (Game.Game (Labyrinth.loadLabyrinth tiles xtile) players 0)

-- Players Part
players :: Parser [Player.Player]
players = many player

player :: Parser Player.Player
player = do color <- color
            control <- control
            position <- position
            cards <- cards
            return $ Player.Player color position cards control

position :: Parser Position.Position
position = do x <- natural
              y <- natural
              return $ Position.Position x y

cards :: Parser [Int]
cards = many natural


color :: Parser Player.Color
color = do  color <- (sat (isColor) `orelse` zero)
            return $ (get color)
            where isColor value = elem value (Utils.toLowerList (map show [Player.Red ..]))
                  get color 
                    | color == "yellow" = Player.Yellow
                    | color == "red" = Player.Red
                    | color == "blue" = Player.Blue
                    | color == "green" = Player.Green

control :: Parser Player.Control
control = do  control <- (sat (isControl) `orelse` zero)
              return $ (get control)
              where isControl value = elem value (Utils.toLowerList  (map show [Player.Human ..]))
                    get control 
                      | control == "ai" = Player.AI
                      | control == "human" = Player.Human

-- tiles

xtile :: Parser Tile.Tile
xtile = do  kind <- kind
            treasure <- treasure
            return $ Tile.Tile Tile.None kind treasure 

-- Use many here was to slow to parse the file
tiles :: Parser [Tile.Tile]
tiles = do  tile' <- tile `orelse` return Tile.Empty
            tiles' <- if tile' /= Tile.Empty 
                        then tiles
                        else do return []
            if tile' /= Tile.Empty 
              then return $ tile':tiles'
              else return $ []


tile :: Parser Tile.Tile
tile = do kind <- kind
          treasure <- treasure
          direction <- direction
          return $ Tile.Tile direction kind treasure

treasure :: Parser Int
treasure = natural `orelse` do return (0)

kind :: Parser Tile.Kind
kind =  do  kind <- (sat (isKind) `orelse` zero)
            return $ (get kind)
            where isKind value = elem value (Utils.toLowerList  (map show [Tile.Corner ..]))
                  get kind 
                    | kind == "corner" = Tile.Corner
                    | kind == "tshape" = Tile.Tshape
                    | kind == "line" = Tile.Line

direction :: Parser Tile.Direction
direction = do  direction <- (sat (isDirection) `orelse` zero)
                return $ (get direction)
                where isDirection value = elem value (Utils.toLowerList  (map show [Tile.North ..]))
                      get direction 
                        | direction == "north" = Tile.North
                        | direction == "south" = Tile.South
                        | direction == "east" = Tile.East
                        | direction == "west" = Tile.West

natural :: Parser Int
natural = do  n <- sat (all isDigit)
              return $ read (n)