module Kojun (solve) where

-- requisitos para o kojun:
---- 1. não pode haver números repetidos em um bloco
---- 2. não pode haver números repetidos em células adjacentes
---- 3. dentro de um bloco, não pode haver um menor em cima de um maior

-- ideia:
---- criar todas as combinações possíveis de tabuleiros preenchidos.
---- para cada uma dessas combinações, verificar se ela é válida através da função verify.
---- a função verify faz um and entre 3 outras:
------ noRepeatedNumberInBlock
------ noRepeatedNumberInAdjacentCells
------ noSmallerNumberOnTopOfBiggerNumber

import Data.Maybe (isNothing, fromMaybe)
import Debug.Trace
import Data.List (nub)

type Board = [[Maybe Int]]

debugTrace :: String -> a -> a
debugTrace msg x = if enableDebug then trace msg x else x

enableDebug :: Bool
enableDebug = False

-- Verifica se um Maybe é Just
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

-- Aplica uma função a um valor dentro de um Maybe
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just (f x)

boardSize :: Board -> Int
boardSize = length

solucaoTeste :: Board
solucaoTeste = [[Just 1, Just 2, Just 1], [Just 2, Just 1, Just 3], [Just 1, Just 3, Just 2]]

solve :: Board -> Board -> IO (Maybe Board)
solve board blocks = do
    debugTrace ("Tentando resolver o tabuleiro:\n" ++ show board) $ return ()
    return $ solveBoard board blocks

solveBoard :: Board -> Board -> Maybe Board
solveBoard board blocks
    | not (verify board blocks) = Nothing
    | allFilled board = Just board
    | otherwise = tryFillCell board blocks nextCell
  where
    nextCell = findEmptyCell board

findEmptyCell :: Board -> Maybe (Int, Int)
findEmptyCell board = 
    let emptyCells = [(r, c) | r <- [0..length board - 1], c <- [0..length (board !! r) - 1], isNothing (board !! r !! c)]
    in if null emptyCells then Nothing else Just (head emptyCells)


tryFillCell :: Board -> Board -> Maybe (Int, Int) -> Maybe Board
tryFillCell _ _ Nothing = Nothing
tryFillCell board blocks (Just (r, c)) =
    let maxValue = maxValueInBlock blocks r c
        possibleValues = [1..maxValue]
        validBoards = [updateBoard board r c (Just v) | v <- possibleValues, verify (updateBoard board r c (Just v)) blocks]
    in tryBoards validBoards blocks

-- Função para determinar o valor máximo permitido em uma célula com base no bloco
maxValueInBlock :: Board -> Int -> Int -> Int
maxValueInBlock blocks r c =
    let blockId = fromMaybe (-1) (blocks !! r !! c)
        blockCells = [(i, j) | i <- [0..length blocks - 1], j <- [0..length (blocks !! i) - 1], blocks !! i !! j == Just blockId]
    in length blockCells


tryBoards :: [Board] -> Board -> Maybe Board
tryBoards [] _ = Nothing
tryBoards (b:bs) blocks =
    case solveBoard b blocks of
        Just solved -> Just solved
        Nothing -> tryBoards bs blocks

updateBoard :: Board -> Int -> Int -> Maybe Int -> Board
updateBoard board r c val =
    take r board ++ [take c (board !! r) ++ [val] ++ drop (c + 1) (board !! r)] ++ drop (r + 1) board

verify :: Board -> Board -> Bool
verify board blocks = and
    [ maxNumEqualsLenBlocks board blocks
    , noRepeatedNumberInBlock board blocks
    , noRepeatedNumberInAdjacentCells board
    , noSmallerNumberOnTopOfBiggerNumber board blocks
    ]

-- Verifica se todas as células estão preenchidas
allFilled :: Board -> Bool
allFilled = all (all isJust)

maxNumEqualsLenBlocks :: Board -> Board -> Bool
maxNumEqualsLenBlocks board blocks = debugTrace ("\nNúmero da célula permitido: " ++ show result ++ "\n") result
  where
    maxBlockValue = maximum [fromMaybe 0 (board !! r !! c) | r <- [0..boardSize board - 1], c <- [0..boardSize board - 1]]
    result = maxBlockValue <= boardSize board


noRepeatedNumberInBlock :: Board -> Board -> Bool
noRepeatedNumberInBlock board blocks = debugTrace ("\nNão há números repetidos: " ++ show result ++ "\n") result
  where
    uniqueBlocks = uniqueElements blocks
    noRepeatsInBlock block = let values = [board !! r !! c | (r, c) <- block, isJust (board !! r !! c)]
                             in debugTrace ("Valores do bloco: " ++ show values) $
                                length values == length (nub values)
    result = all noRepeatsInBlock uniqueBlocks

uniqueElements :: Eq a => [[a]] -> [[(Int, Int)]]
uniqueElements xss = debugTrace "\nProcurando elementos únicos\n" $
    map (\x -> [(r, c) | r <- [0..length xss - 1], c <- [0..length (xss !! r) - 1], xss !! r !! c == x]) (nub (concat xss))

-- Verifica se há números repetidos nas células adjacentes
noRepeatedNumberInAdjacentCells :: Board -> Bool
noRepeatedNumberInAdjacentCells board = debugTrace ("\nNão há celulas adjacentes com valores iguais: " ++ show result ++ "\n") result
  where
    noRepeatsInAdjacentCells (r, c) =
        let cellValue = board !! r !! c
            adjacentPositions = [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]
            adjacentValues = [board !! r' !! c' | (r', c') <- adjacentPositions, r' >= 0, r' < length board, c' >= 0, c' < length (board !! r')]
        in debugTrace ("Célula (" ++ show r ++ ", " ++ show c ++ ") valor: " ++ show cellValue ++ ", Valores adjacentes: " ++ show adjacentValues) $
           isNothing cellValue || notElem cellValue adjacentValues
    result = all noRepeatsInAdjacentCells [(r, c) | r <- [0..length board - 1], c <- [0..length (board !! r) - 1]]

-- Verifica se dentro de um bloco, não há um menor em cima de um maior
noSmallerNumberOnTopOfBiggerNumber :: Board -> Board -> Bool
noSmallerNumberOnTopOfBiggerNumber board blocks =
    debugTrace ("\nVerificando números menores em cima de maiores no mesmo bloco: " ++ show result) result
  where
    -- Função que verifica a regra para uma célula específica
    isValidCell (i, j) =
        let currentValue = board !! i !! j
            aboveValue = if i > 0 && (blocks !! i !! j == blocks !! (i - 1) !! j)
                         then board !! (i - 1) !! j
                         else Nothing
        in debugTrace ("Célula (" ++ show i ++ ", " ++ show j ++ ") valor: " ++ show currentValue ++ ", Acima: " ++ show aboveValue) $
           case (currentValue, aboveValue) of
               (Just curr, Just above) -> curr <= above -- Regra corrigida: atual deve ser menor ou igual ao de cima
               _ -> True -- Caso não haja valores ou estejam em blocos diferentes

    -- Verifica todas as células do tabuleiro
    result = all isValidCell [(i, j) | i <- [0 .. boardSize board - 1], j <- [0 .. boardSize board - 1]]
