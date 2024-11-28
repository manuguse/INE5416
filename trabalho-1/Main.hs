module Main where

import System.Random (randomRIO)
import Parser (parseFileToMatrix)
import Kojun (solve)

-- Lista de boards disponíveis
getAvailableBoards :: [Int]
getAvailableBoards = [1, 8, 14]

-- Escolhe um board aleatoriamente (a fim de testes, começaremos escolhendo sempre o 1)
getRandomNumberFromAvailable :: [Int] -> IO Int
getRandomNumberFromAvailable available = do
    -- randomIndex <- randomRIO (0, length available - 1)
    -- return $ available !! randomIndex
    return 1

main :: IO ()
main = do
    number <- getRandomNumberFromAvailable getAvailableBoards
    let boardPath = "boards/" ++ show number ++ ".txt"
    let blockPath = "blocks/" ++ show number ++ ".txt"

    values <- parseFileToMatrix boardPath
    groups <- parseFileToMatrix blockPath

    case solve values groups of
        Nothing -> putStrLn "No solution found!"
        Just solution -> do
            putStrLn "Solution:"
            mapM_ print solution
