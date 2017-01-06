{-#LANGUAGE ScopedTypeVariables#-}
import System.Random
import Control.Monad
import Data.Char

import qualified Labyrinth
import qualified Grammar
import qualified Utils

main :: IO ()
main = do
    g <- getStdGen
    let randomList = (randomRs (0, 1) g) :: [Float]
    let labyrinth = Labyrinth.initLabyrinth randomList
    let players = Labyrinth.initPlayers
    putStrLn $ show (labyrinth)
    putStrLn $ show (players) ++ "\n"
    let (labyrinth2, players2, bool) = (Labyrinth.putExtraTile (Labyrinth.Position 7 2) labyrinth players)
    putStrLn $ show (labyrinth2)
    putStrLn $ show (players2)

    -- putStrLn $ "What do you want to do ? (P = play, S = save, Q = quit)"
    -- action <- getLine
    -- -- action <- return ('S')
    -- when (action /= "Q") $ do
    --     playerNbr <- getNumber validPlayerNbr "How many players will play (1 to 4 ?)"
    --     putStrLn $ show playerNbr ++ " players"

    -- putStrLn $ show createLabyrinth

getNumber :: (Int -> Bool) -> String -> IO Int
getNumber validator question = do
    putStrLn question
    (number :: Int) <- readLn

    if validator number
        then return (number)
        else do
            putStrLn $ "The number of player must be digit between 1 and 4"
            getNumber validator question
            

validPlayerNbr :: Int -> Bool
validPlayerNbr number
    | number < 1 = False
    | number > 4 = False
    | otherwise = True
