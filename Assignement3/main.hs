{-#LANGUAGE ScopedTypeVariables#-}
import System.Random
import Control.Monad
import Data.Char
import System.Process

import qualified System.Environment

import qualified Parser
import qualified Grammar
import qualified Utils

import qualified Game
import qualified Labyrinth
import qualified Player
import qualified Position
import qualified Tile

main :: IO ()
main = do
    args <- System.Environment.getArgs
    
    g <- getStdGen
    let randomList = (randomRs (0, 1) g) :: [Float]

    game <- initGame args randomList
    putStrLn $ "Bye !"

initGame :: [String] -> [Float] -> IO Game.Game
initGame args randomList = do
    if length args == 1
        then do savedGame <- readFile (args !! 0)
                let loadedGame = Parser.parser Grammar.labyrinth (words savedGame)
                runGame loadedGame randomList
    else
        do playersNumber <- askPlayer 
           controls <- askAI playersNumber
           let newGame = Game.initGame controls randomList
           runGame newGame randomList

runGame :: Game.Game -> [Float] -> IO Game.Game
runGame game randomList = do
    startTurn game randomList

startTurn :: Game.Game -> [Float] -> IO Game.Game
startTurn game randomList = do
    let (isFinish, winner) = Game.isFinish game
    if isFinish
        then do
             putStrLn $ "Game Won by " ++ show (winner)
             return game
        else
            if Player.isAI (Game.getCurrentPlayer game)
                then do
                    playTurnAI game randomList
                else
                    do
                    putStrLn $ "It is " ++ show (Player.color (Game.getCurrentPlayer game)) ++ "'s turn !"
                    putStrLn $ "What do you want to do ? (P = play, Any = save and quit)"
                    action <- getLine         
                    if action /= "P"
                        then do 
                            saveFile game
                            return game
                    else do newGame <- playTurn game randomList
                            return newGame



playTurnAI :: Game.Game -> [Float] -> IO Game.Game
playTurnAI game randomList = do
    let (gameAfterPuttingTile, randomAfterPuttingTile) = Game.putExtraTileAI randomList game
    let (gameAfterGatheringTreasures, _) = Game.gatherTreasures gameAfterPuttingTile
    let (gameAfterMovingPawn, randomAfterMovingPawn) = Game.movePawnAI randomAfterPuttingTile gameAfterGatheringTreasures
    gameAfterTurn <- nextPlayer gameAfterMovingPawn
    -- putStrLn $ show gameAfterTurn
    endGame <- startTurn gameAfterTurn randomAfterMovingPawn
    return (endGame)

playTurn :: Game.Game -> [Float] -> IO Game.Game
playTurn game randomList = do
    putStrLn $ show (game)

    gameAfterPuttingTile <- putExtraTile game
    putStrLn $ show (gameAfterPuttingTile)

    gameAfterGatheringTreasures <- gatherTreasures gameAfterPuttingTile

    gameAfterMovingPawn <- movePawn gameAfterGatheringTreasures
    putStrLn $ show (gameAfterMovingPawn)

    gameAfterTurn <- nextPlayer gameAfterMovingPawn

    -- Add winning situation
    putStrLn $ "Press any key to switch player."
    continue <- getLine

    System.Process.callCommand "clear"
    endGame <- startTurn gameAfterTurn randomList
    return (endGame)

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
            movePawn game

gatherTreasures :: Game.Game -> IO Game.Game
gatherTreasures game = do
    let (newGame, treasures) = Game.gatherTreasures game
    putStrLn $ "Wow you manage to gather thoses treasures : " ++ show treasures
    return (newGame)

nextPlayer :: Game.Game -> IO Game.Game
nextPlayer game = do
    let newGame = Game.nextPlayer game
    return (newGame)

    
askPosition :: IO Position.Position
askPosition = do
    x <- getNumber validPosition "X: " "X must be digit between 1 and 7"
    y <- getNumber validPosition "Y: " "Y must be digit between 1 and 7"
    return (Position.Position x y)

validPosition :: Int -> Bool
validPosition number
    | number < 1 = False
    | number > 7 = False
    | otherwise = True

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
            
askPlayer :: IO Int
askPlayer = do
    getNumber validPosition "How many players will take part of the game ? (1 to 4) " "Number of players must be digit between 1 and 4"

validPlayerNbr :: Int -> Bool 
validPlayerNbr number
    | number < 1 = False
    | number > 4 = False
    | otherwise = True

askAI :: Int -> IO [Player.Control]
askAI nbrPlayers = askAI' 0 nbrPlayers []

askAI' :: Int -> Int -> [Player.Control] -> IO [Player.Control]
askAI' id nbrPlayer controls = do
    if id == nbrPlayer then return controls
    else do
        let color = [Player.Red ..]
        putStrLn $ "Will be " ++ show (color !! id) ++ " (A)I or (H)uman ? "
        control <- getLine
        if control /= "A" && control /= "H" then
            do
            putStrLn $ "This kind of player doesn't exist."
            askAI' id nbrPlayer controls
        else
            do
            let controlType = if control == "A" then Player.AI else Player.Human
            askAI' (id+1) nbrPlayer (controls ++ [controlType])

saveFile :: Game.Game -> IO ()
saveFile game = do
    putStrLn $ "Where do you want to save the game ? "
    filename <- getLine
    writeFile filename (Game.save game)

