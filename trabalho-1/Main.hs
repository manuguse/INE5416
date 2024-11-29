module Main where

import System.Random (randomRIO)
import Kojun (solve)
import Parser (parseFileToMatrix, printBoard)

-- Lista de boards disponíveis
getAvailableBoards :: [Int]
getAvailableBoards = [0, 1, 8, 14]


getRandomNumberFromAvailable available = do
    -- randomIndex <- randomRIO (0, length available - 1)
    -- return $ available !! randomIndex
    return 0

main :: IO ()
main = do
    number <- getRandomNumberFromAvailable getAvailableBoards
    let boardPath = "boards/" ++ show number ++ ".txt"
    let blockPath = "blocks/" ++ show number ++ ".txt"

    board <- parseFileToMatrix boardPath
    blocks <- parseFileToMatrix blockPath

    putStrLn "Tabuleiro inicial:"
    printBoard board

    solution <- solve board blocks
    case solution of
        Nothing -> putStrLn "Não foi possível encontrar uma solução."
        Just sol -> do
            putStrLn "\nSolução:"
            printBoard sol