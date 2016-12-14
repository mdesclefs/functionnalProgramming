import qualified System.Environment
 
main :: IO ()
main = do [path] <- System.Environment.getArgs
          maze <- readFile path
          putStr $ unlines $ escape $ lines maze
 
escape :: [[Char]] -> [[Char]]
escape maze = fst ( escape' 1 1 maze )
 
-- Recursive helper function for escaping the maze
escape' :: Int -> Int -> [[Char]] -> ([[Char]], Bool)
escape' x y maze
   | not (isAvailableMove x y maze) = (maze, False)
   | isEndingMove x y maze = (maze, True)
   | snd north = north
   | snd south = south
   | snd west = west
   | snd east = east
   | otherwise = east
   -- Each next step should use previous marked path
   where   north = (escape' x (y-1) (drawPath x y maze))
            south = (escape' x (y+1) (drawPath x y (fst north)))
           west = (escape' (x-1) y (drawPath x y (fst south)))
            east = (escape' (x+1) y (drawPath x y (fst west)))
 
isAvailableMove :: Int -> Int -> [[Char]] -> Bool
isAvailableMove x y maze
   | ((maze !! y) !! x) == '.' = False
   | ((maze !! y) !! x) == 'X' = False
   | otherwise = True
 
isBeginningMove :: Int -> Int -> [[Char]] -> Bool
isBeginningMove x y maze
   | ((maze !! y) !! x) == '*' = True
   | otherwise = False
 
isEndingMove :: Int -> Int -> [[Char]] -> Bool
isEndingMove x y maze
   | ((maze !! y) !! x) == '@' = True
   | otherwise = False
 
drawPath :: Int -> Int -> [[Char]] -> [[Char]]
drawPath x y maze
   | isBeginningMove x y maze = maze
   | otherwise =   take y maze ++
                   [take x (maze !! y) ++ ['.'] ++ drop (x + 1) (maze !! y)] ++
                   drop (y + 1) maze