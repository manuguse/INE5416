module Parser (parseFileToMatrix, printBoard) where

import Data.Maybe (fromMaybe, isNothing)
import Data.List (nub)
import System.Console.ANSI (setSGR, SGR(..), Color(..), ColorIntensity(..), ConsoleLayer(..))

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

-- lista de cores disponíveis
colors :: [Color]
colors = [Red, Green, Yellow, Blue, Magenta, Cyan, White]

-- associa valores únicos a cores
getColorMap :: [[Maybe Int]] -> [(Int, Color)]
getColorMap board = zip uniqueValues (cycle colors)
  where
    uniqueValues = nub [x | Just x <- concat board]

-- imprime o tabuleiro colorido
-- imprime o tabuleiro colorido com base no `block`
printBoard :: [[Maybe Int]] -> [[Maybe Int]] -> IO ()
printBoard board block = do
    let colorMap = getColorMap block
    mapM_ (printRow colorMap) (zip board block)
  where
    printRow :: [(Int, Color)] -> ([Maybe Int], [Maybe Int]) -> IO ()
    printRow colorMap (row, blockRow) = do
        mapM_ (printCell colorMap) (zip row blockRow)
        putStrLn ""

    printCell :: [(Int, Color)] -> (Maybe Int, Maybe Int) -> IO ()
    printCell colorMap (Nothing, Just blockValue) = do
        let color = lookup blockValue colorMap
        case color of
            Just c -> do
                setSGR [SetColor Foreground Vivid c]
                putStr "◻ "
                setSGR [Reset]
            Nothing -> putStr "◻ "
    printCell _ (Nothing, Nothing) = putStr "◻ "
    printCell colorMap (Just value, Just blockValue) = do
        let color = lookup blockValue colorMap
        case color of
            Just c -> do
                setSGR [SetColor Foreground Vivid c]
                putStr $ show value ++ " "
                setSGR [Reset]
            Nothing -> putStr $ show value ++ " "
    printCell _ (Just value, Nothing) = putStr $ show value ++ " "