module Parser (parseFileToMatrix, printBoard) where

import Data.Maybe (fromMaybe)

-- transforma uma linha do arquivo em uma lista de Maybe Int
parseLine :: String -> [Maybe Int]
parseLine = map parseChar . words
  where
    parseChar "-" = Nothing
    parseChar x   = Just (read x :: Int)

-- lê o conteúdo do arquivo e converte para uma matriz de Maybe Int
parseFileToMatrix :: FilePath -> IO [[Maybe Int]]
parseFileToMatrix filePath = do
    content <- readFile filePath
    let linesOfFile = lines content
    return $ map parseLine linesOfFile

printBoard :: [[Maybe Int]] -> IO ()
printBoard = mapM_ printRow
  where
    printRow row = do
        mapM_ printCell row
        putStrLn ""
    printCell cell = putStr $ maybe "◻" show cell ++ " "