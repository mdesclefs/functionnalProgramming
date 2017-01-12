{-#LANGUAGE ScopedTypeVariables#-}
import System.Random
import Control.Monad
import Data.Char

import qualified Utils
import qualified Game
import qualified Labyrinth
import qualified Grammar
import qualified Player
import qualified Position
import qualified Tile

main :: IO ()
main = do
    g <- getStdGen
    let randomList = (randomRs (0, 1) g) :: [Float]
    let game = Game.initGame randomList
    putStrLn $ show (game)
    let labyrinth = Game.labyrinth game
    let players = Game.players game
    let current_player = Game.current_player game
    putStrLn $ show (Labyrinth.reachablePositions (players !! current_player) labyrinth)
    -- runGame (Game.initGame randomList)
    putStrLn $ "Bye !"

    -- let labyrinth = Labyrinth.initLabyrinth randomList
    -- let players = Player.initPlayers randomList
    -- putStrLn $ show (labyrinth)
    -- putStrLn $ show (players) ++ "\n"

    -- let (labyrinth2, players2, bool) = (Labyrinth.putExtraTile (Position.Position 7 2) Tile.North labyrinth players)
    -- putStrLn $ show (labyrinth2)
    -- putStrLn $ show (players2)
    -- putStrLn $ "What do you want to do ? (P = play, S = save, Q = quit)"
    -- action <- getLine
    -- -- action <- return ('S')
    -- when (action /= "Q") $ do
    --     playerNbr <- getNumber validPlayerNbr "How many players will play (1 to 4 ?)" "The number of player must be digit between 1 and 4"
    --     putStrLn $ show playerNbr ++ " players"

    -- putStrLn $ show createLabyrinth

runGame :: Game.Game -> IO String
runGame game = do
    putStrLn $ "What do you want to do ? (P = play, S = save and quit)"
    action <- getLine
    if action == "Q"
        then do putStrLn $ "Quit"
        else do game <- playTurn game
                putStrLn game
    return("Bye !")

playTurn :: Game.Game -> IO (String)
playTurn game = do
    putStrLn $ show (game)
    gameAfterPuttingTile <- putExtraTile game
    putStrLn $ show (gameAfterPuttingTile)
    putStrLn $ "Woooow u got those treasures : []"
    gameAfterMovingPawn <- movePawn gameAfterPuttingTile
    putStrLn $ show (gameAfterMovingPawn)
    -- Labyrinth.putExtraTile position direction game
    return ("Nice")

putExtraTile :: Game.Game -> IO Game.Game
putExtraTile game = do
    putStrLn $ "Where do you want to put the extra tile ?"
    position <- askPosition
    direction <- askDirection
    let (newGame, result) = Game.putExtraTile position direction game
    if result
        then return (newGame)
        else do 
            putStrLn $ "Error: Either the position is invalid, either a player's pawn will be out of the board."
            putExtraTile game

movePawn :: Game.Game -> IO Game.Game
movePawn game = do
    putStrLn $ "Where do you want to go ?"
    nextPosition <- askPosition
    let (newGame, result) = Game.movePawn nextPosition game
    if result
        then return (newGame)
        else do 
            putStrLn $ "Error: This position can not be reach by your pawn."
            putExtraTile game

    
askPosition :: IO Position.Position
askPosition = do
    x <- getNumber validPosition "X: " "X must be digit between 1 and 7"
    y <- getNumber validPosition "Y: " "Y must be digit between 1 and 7"
    return (Position.Position x y)

askDirection :: IO Tile.Direction
askDirection = do
    putStrLn $ "In which direction (N=North, S=South, E=East, W=West) ?"
    direction <- getLine

    if elem direction ["N", "S", "E", "W"]
        then return (getDirection direction)
        else do
            putStrLn "Wrong direction"
            askDirection

getDirection :: String -> Tile.Direction
getDirection direction
    | direction == "N" = Tile.North
    | direction == "S" = Tile.South
    | direction == "E" = Tile.East
    | direction == "W" = Tile.West

getNumber :: (Int -> Bool) -> String -> String -> IO Int
getNumber validator question fail = do
    putStr question
    (number :: Int) <- readLn

    if validator number
        then return (number)
        else do
            putStrLn $ fail
            getNumber validator question fail
            
validPosition :: Int -> Bool
validPosition number
    | number < 1 = False
    | number > 7 = False
    | otherwise = True

validPlayerNbr :: Int -> Bool
validPlayerNbr number
    | number < 1 = False
    | number > 4 = False
    | otherwise = True
