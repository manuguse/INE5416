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
enableDebug = True

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
    debugTrace ("\nBoard: " ++ show board) $ return ()
    debugTrace ("Blocks: " ++ show blocks) $ return ()
    -- debugTrace ("Solucao: " ++ show solucao) $ return ()
    if verify solucaoTeste blocks
    then return (Just solucaoTeste)
    else return Nothing

verify :: Board -> Board -> Bool
verify board blocks = debugTrace ("Board válido: " ++ show result) result
  where
    result = and [maxNumEqualsLenBlocks board blocks, allFilled board, noRepeatedNumberInBlock board blocks, noRepeatedNumberInAdjacentCells board, noSmallerNumberOnTopOfBiggerNumber board blocks]

allFilled :: Board -> Bool
allFilled board = debugTrace ("Todas as células estão preenchidas: " ++ show result) result
  where
    result = all (all isJust) board

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
