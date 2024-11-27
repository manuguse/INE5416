module Main (main) where
import System.Random (randomRIO)
    
getAvailableBoards :: [Int] 
getAvailableBoards = [1, 8, 14]

getRandomNumberFromAvailable :: [Int] -> IO Int
getRandomNumberFromAvailable available = do
    randomIndex <- randomRIO (0, length available - 1)
    return $ available !! randomIndex

main :: IO ()
main = do
    number <- getRandomNumberFromAvailable getAvailableBoards
    let board = "boards/" ++ show number ++ ".txt"
    let block = "blocks/" ++ show number ++ ".txt"
    
    putStrLn $ "Board: " ++ board
    putStrLn $ "Block: " ++ block