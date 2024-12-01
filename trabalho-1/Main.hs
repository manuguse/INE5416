module Main where

import System.Random (randomRIO)
import Kojun (solve)
import Parser (parseFileToMatrix, printBoard)
import Control.Monad (forM_)

-- Lista de boards disponíveis
getAvailableBoards :: [Int]
getAvailableBoards = [0, 1, 8, 14]

main :: IO ()
main = do
    let availableBoards = getAvailableBoards
    forM_ availableBoards $ \number -> do
        putStrLn "\n--------------------------------\n"
        let boardPath = "boards/" ++ show number ++ ".txt"
        let blockPath = "blocks/" ++ show number ++ ".txt"

        board <- parseFileToMatrix boardPath
        blocks <- parseFileToMatrix blockPath

        putStrLn $ "Tabuleiro inicial (" ++ show number ++ "):"
        printBoard board

        solution <- solve board blocks
        case solution of
            Nothing -> putStrLn "Não foi possível encontrar uma solução."
            Just sol -> do
                putStrLn "\nSolução:"
                printBoard sol
