module Position where 

-- Position Definition
data Position = Position { x :: Int
                         , y :: Int
                         } deriving (Eq, Read)

instance Show Position where
    show (Position x y) =  "(" ++ show (x+1) ++ "," ++ show (y+1) ++ ")"