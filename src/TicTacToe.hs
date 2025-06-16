-- tic_tac_toe.hs

import Data.List (intercalate)
import Data.Char (isDigit)
import Text.Read (readMaybe)

-- Data Types
data Player = X | O deriving (Show, Eq)

data Cell = Empty Int | Occupied Player deriving (Eq)

instance Show Cell where
    show (Empty n) = show n
    show (Occupied p) = show p

type Board = [Cell]

-- Game Logic Functions
initializeBoard :: Board
initializeBoard = [Empty i | i <- [1..9]]

displayBoard :: Board -> IO ()
displayBoard board = do
    putStrLn "\n--- Tic-Tac-Toe Board ---"
    putStrLn "-------------"
    let rows = [take 3 (drop (i*3) board) | i <- [0..2]]
    mapM_ (\row -> do
        putStrLn $ "| " ++ intercalate " | " (map show row) ++ " |"
        putStrLn "-------------") rows
    putStrLn "-------------------------\n"

checkWin :: Board -> Player -> Bool
checkWin board player = any (checkLine board player) winConditions
  where
    winConditions =
        [ [0, 1, 2], [3, 4, 5], [6, 7, 8]
        , [0, 3, 6], [1, 4, 7], [2, 5, 8]
        , [0, 4, 8], [2, 4, 6]
        ]
    checkLine b p [i1, i2, i3] =
        (b !! i1 == Occupied p) && (b !! i2 == Occupied p) && (b !! i3 == Occupied p)
    checkLine _ _ _ = False

checkDraw :: Board -> Bool
checkDraw board = all isOccupied board
  where
    isOccupied (Occupied _) = True
    isOccupied (Empty _) = False

getPlayerMove :: Player -> IO Int
getPlayerMove currentPlayer = do
    putStrLn $ "Player " ++ show currentPlayer ++ "'s turn."
    putStr "Enter your move (1-9): "
    line <- getLine
    case readMaybe line :: Maybe Int of
        Just choice | choice >= 1 && choice <= 9 -> return choice
        _ -> do
            putStrLn "Invalid input. Please enter a number from 1 to 9."
            getPlayerMove currentPlayer

makeMove :: Board -> Player -> Int -> Maybe Board
makeMove board player choice =
    let index = choice - 1
    in if index >= 0 && index < length board
       then case board !! index of
                Empty _ -> Just $ take index board ++ [Occupied player] ++ drop (index + 1) board
                Occupied _ -> Nothing
       else Nothing

playGame :: Board -> Player -> IO ()
playGame board currentPlayer = do
    displayBoard board

    if checkWin board currentPlayer then do
        putStrLn $ "Player " ++ show currentPlayer ++ " wins! Congratulations!"
        putStrLn "Game over. Thanks for playing!"
    else if checkDraw board then do
        putStrLn "It's a draw!"
        putStrLn "Game over. Thanks for playing!"
    else do
        choice <- getPlayerMove currentPlayer
        case makeMove board currentPlayer choice of
            Just newBoard ->
                let nextPlayer = if currentPlayer == X then O else X
                in playGame newBoard nextPlayer
            Nothing -> do
                putStrLn "Invalid move. That cell is already taken or out of bounds. Please choose an empty cell."
                playGame board currentPlayer

main :: IO ()
main = do
    putStrLn "Welcome to Tic-Tac-Toe!"
    putStrLn "Players use 'X' and 'O'. Enter the number of the cell (1-9) to place your mark."
    let initialBoard = initializeBoard
    playGame initialBoard X
