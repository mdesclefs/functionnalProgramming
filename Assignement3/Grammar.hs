module Grammar where 

import Data.Char
import Parser
import qualified Control.Monad

import qualified Labyrinth
import qualified Player
import qualified Tile
import qualified Position

labyrinth :: Parser String
labyrinth = do  players <- players
                xtile <- xtile
                -- tiles <- tiles
                return $ show "brabra"

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
color = do  color <- item
            return $ (get color)
            where get color 
                    | color == "yellow" = Player.Yellow
                    | color == "red" = Player.Red
                    | color == "blue" = Player.Blue
                    | color == "green" = Player.Green

control :: Parser Player.Control
control = do  control <- item
              return $ (get control)
              where get control 
                      | control == "ai" = Player.AI
                      | control == "human" = Player.Human

-- tiles

xtile :: Parser Tile.Tile
xtile = do  kind <- kind
            treasure <- treasure
            return $ Tile.Tile Tile.None kind treasure 

tiles :: Parser [Tile.Tile]
tiles = many tile

-- tiles :: Parser [Tile.Tile]
-- tiles = do  t <- (tile `orelse` return null)
--             ts <- if (t == null) then return [] else tiles
--             return (if (t == null) then ts else (t:ts))

tile :: Parser Tile.Tile
tile = do kind <- kind
          treasure <- treasure
          direction <- direction
          return $ Tile.Tile direction kind treasure

treasure :: Parser Int
treasure = natural `orelse` do return (0)

kind :: Parser Tile.Kind
kind =  do  kind <- item
            return $ (get kind)
            where get kind 
                    | kind == "corner" = Tile.Corner
                    | kind == "tshape" = Tile.Tshape
                    | kind == "line" = Tile.Line

direction :: Parser Tile.Direction
direction = do  direction <- item
                return $ (get direction)
                where get direction 
                        | direction == "north" = Tile.North
                        | direction == "south" = Tile.South
                        | direction == "east" = Tile.East
                        | direction == "west" = Tile.West

-- natural :: Parser Int
-- natural = do {  n <- value;
--                 return (n)
--             }

natural :: Parser Int
natural = do  n <- sat (all isDigit)
              return $ read (n)

main :: IO ()
main = do
        content <- readFile "labyrinth"
        putStrLn $ show $ apply labyrinth (words content)
