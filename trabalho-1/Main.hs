module Main where

import Kojun (solve)
import Parser (parseFileToMatrix, printBoard)
import Control.Monad (forM_)

-- lista de boards disponíveis
getAvailableBoards :: [Int]
--getAvailableBoards = [0, 1, 8, 14]
getAvailableBoards = [0..35] 

main :: IO ()
main = do
    let availableBoards = getAvailableBoards -- recebe a lista de boards disponíveis
    forM_ availableBoards $ \number -> do -- para cada board disponível,
        putStrLn "\n--------------------------------\n"
        -- define o caminho dos arquivos de tabuleiro e blocos
        let boardPath = "boards/" ++ show number ++ ".txt" 
        let blockPath = "blocks/" ++ show number ++ ".txt"

        -- lê os arquivos de tabuleiro e blocos, fazendo o parse para uma matriz
        board <- parseFileToMatrix boardPath
        blocks <- parseFileToMatrix blockPath

        -- printa o tabuleiro inicial
        putStrLn $ "Tabuleiro inicial (" ++ show number ++ "):"
        printBoard board blocks

        -- tenta resolver o tabuleiro, chamando a função solve de Kojun
        solution <- solve board blocks
        case solution of
            Nothing -> putStrLn "Não foi possível encontrar uma solução."
            Just sol -> do
                putStrLn "\nSolução:"
                printBoard sol blocks
