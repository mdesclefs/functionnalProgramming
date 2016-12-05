module Utils 
(
    Position,
    Board
) where

-- Position Definition

data Position = Position { x :: Int
                         , y :: Int
                         }

instance Show (Position) where
    show position =  "(" ++ show (x position) ++ "," ++ show (y position) ++ ")"

-- Board Definition

-- data Board = Board [[Char]] deriving (Read)
data Board = Board  { width :: Int
                    , height :: Int
                    , entrance :: Position
                    , exists :: [Position]
                    , walls :: [Position]
                    }
