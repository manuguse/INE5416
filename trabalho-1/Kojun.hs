module Kojun (solve) where

-- requisitos para o kojun:
---- 1. não pode haver números repetidos em um bloco
---- 2. não pode haver números repetidos em células adjacentes
---- 3. dentro de um bloco, não pode haver um menor em cima de um maior

-- ideia:
---- criar todas as combinações possíveis de tabuleiros preenchidos.
---- para cada uma dessas combinações, verificar se ela é válida através da função verify.
---- a função verify faz um and entre 4 outras:
------ noRepeatedNumberInBlock
------ noRepeatedNumberInAdjacentCells
------ noSmallerNumberOnTopOfBiggerNumber
------ maxNumEqualsLenBlocks

import Data.Maybe (isNothing, fromMaybe)
import Debug.Trace
import Data.List (nub)

type Board = [[Maybe Int]] -- o tipo board será usado para board e blocks, representando uma matriz de Maybe Int

-- função auxiliar que printa (debug) ou não de acordo com a variável enableDebug
debugTrace :: String -> a -> a
debugTrace msg x = if enableDebug then trace msg x else x

enableDebug :: Bool
enableDebug = False

-- função que verifica se um Maybe é Just, o que é usado para verificar se uma célula está preenchida
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False


-- aplica uma função a um valor, retornano Nothing se o valor for Nothing
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just (f x)

-- retorna o tamanho n de um tabuleiro n x n
boardSize :: Board -> Int
boardSize = length

-- função que resolve o tabuleiro chamando a função solveBoard
solve :: Board -> Board -> IO (Maybe Board)
solve board blocks = do
    debugTrace ("Tentando resolver o tabuleiro:\n" ++ show board) $ return ()
    return $ solveBoard board blocks

-- verifica se o tabuleiro é válido usando a função verify.
-- se o tabuleiro não for válido, retorna Nothing.
-- se o tabuleiro estiver completamente preenchido, retorna o tabuleiro.
-- caso contrário, tenta preencher a próxima célula vazia (nextCell) chamando a função tryFillCell.
solveBoard :: Board -> Board -> Maybe Board
solveBoard board blocks = debugTrace ("solveBoard chamado com board:\n" ++ show board) $
    if not (verify board blocks)
    then debugTrace "Tabuleiro inválido" Nothing
    else if allFilled board
         then debugTrace "Tabuleiro preenchido corretamente" (Just board)
         else tryFillCell board blocks nextCell
  where
    nextCell = findEmptyCell board

-- procura a primeira célula vazia em um tabuleiro e cria uma lista de coordenadas de células vazias (emptyCells)
findEmptyCell :: Board -> Maybe (Int, Int)
findEmptyCell board = 
    let emptyCells = [(r, c) | r <- [0..length board - 1], c <- [0..length (board !! r) - 1], isNothing (board !! r !! c)]
    in debugTrace ("Células vazias encontradas: " ++ show emptyCells) $
       if null emptyCells then Nothing else Just (head emptyCells)

-- tenta preencher uma célula vazia com valores possíveis (possibleValues) e verifica se o tabuleiro é válido.
tryFillCell :: Board -> Board -> Maybe (Int, Int) -> Maybe Board
tryFillCell _ _ Nothing = debugTrace "Nenhuma célula vazia encontrada" Nothing
tryFillCell board blocks (Just (r, c)) = debugTrace ("Tentando preencher célula (" ++ show r ++ ", " ++ show c ++ ")") $
    let maxValue = maxValueInBlock blocks r c
        possibleValues = [1..maxValue]
        validBoards = [updateBoard board r c (Just v) | v <- possibleValues, verify (updateBoard board r c (Just v)) blocks]
    in tryBoards validBoards blocks

-- calcula o valor máximo permitido para uma célula com base no bloco (será o próprio taamanho do bloco)
maxValueInBlock :: Board -> Int -> Int -> Int
maxValueInBlock blocks r c = debugTrace ("Calculando valor máximo para célula (" ++ show r ++ ", " ++ show c ++ ")") $
    let blockId = fromMaybe (-1) (blocks !! r !! c)
        blockCells = [(i, j) | i <- [0..length blocks - 1], j <- [0..length (blocks !! i) - 1], blocks !! i !! j == Just blockId]
    in length blockCells

-- tenta resolver o tabuleiro para cada uma das possíveis configurações válidas
tryBoards :: [Board] -> Board -> Maybe Board
tryBoards [] _ = debugTrace "Nenhum tabuleiro válido encontrado" Nothing
tryBoards (b:bs) blocks = debugTrace ("Tentando resolver tabuleiro:\n" ++ show b) $
    case solveBoard b blocks of
        Just solved -> debugTrace "Tabuleiro resolvido" (Just solved)
        Nothing -> tryBoards bs blocks

-- atualiza o tabuleiro com um novo valor em uma célula específica
updateBoard :: Board -> Int -> Int -> Maybe Int -> Board
updateBoard board r c val =
    take r board ++ [take c (board !! r) ++ [val] ++ drop (c + 1) (board !! r)] ++ drop (r + 1) board

-- verifica se o tabuleiro é válido de acordo com as regras do jogo
verify :: Board -> Board -> Bool
verify board blocks = debugTrace ("Verificando tabuleiro:\n" ++ show board) $
    and [ maxNumEqualsLenBlocks board blocks
    , noRepeatedNumberInBlock board blocks
    , noRepeatedNumberInAdjacentCells board
    , noSmallerNumberOnTopOfBiggerNumber board blocks
    ]

-- verifica se todas as células do tabuleiro estão preenchidas
allFilled :: Board -> Bool
allFilled board = debugTrace ("Verificando se todas as células estão preenchidas:\n" ++ show board) $
    all (all isJust) board -- verifica se todas as células são Just

-- verifica se o maior número no bloco é menor ou igual ao tamanho do bloco
maxNumEqualsLenBlocks :: Board -> Board -> Bool
maxNumEqualsLenBlocks board blocks = debugTrace ("\nNúmero da célula permitido: " ++ show result ++ "\n") result
  where
    maxBlockValue = maximum [fromMaybe 0 (board !! r !! c) | r <- [0..boardSize board - 1], c <- [0..boardSize board - 1]]
    result = maxBlockValue <= boardSize board

-- verifica se não há números repetidos em um bloco
noRepeatedNumberInBlock :: Board -> Board -> Bool
noRepeatedNumberInBlock board blocks = debugTrace ("\nNão há números repetidos: " ++ show result ++ "\n") result
  where
    uniqueBlocks = uniqueElements blocks
    noRepeatsInBlock block = let values = [board !! r !! c | (r, c) <- block, isJust (board !! r !! c)]
                             in debugTrace ("Valores do bloco: " ++ show values) $
                                length values == length (nub values)
    result = all noRepeatsInBlock uniqueBlocks

-- retorna uma lista de coordenadas únicas para cada bloco
uniqueElements :: Eq a => [[a]] -> [[(Int, Int)]]
uniqueElements xss = debugTrace "\nProcurando elementos únicos\n" $
    map (\x -> [(r, c) | r <- [0..length xss - 1], c <- [0..length (xss !! r) - 1], xss !! r !! c == x]) (nub (concat xss))

-- verifica se não há números repetidos em células adjacentes
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

-- verifica se não há números menores em cima de números maiores no mesmo bloco
noSmallerNumberOnTopOfBiggerNumber :: Board -> Board -> Bool
noSmallerNumberOnTopOfBiggerNumber board blocks =
    debugTrace ("\nVerificando números menores em cima de maiores no mesmo bloco: " ++ show result) result
  where
    isValidCell (i, j) =
        let currentValue = board !! i !! j
            aboveValue = if i > 0 && (blocks !! i !! j == blocks !! (i - 1) !! j)
                         then board !! (i - 1) !! j
                         else Nothing
        in debugTrace ("Célula (" ++ show i ++ ", " ++ show j ++ ") valor: " ++ show currentValue ++ ", Acima: " ++ show aboveValue) $
           case (currentValue, aboveValue) of
               (Just curr, Just above) -> curr <= above 
               _ -> True 

    result = all isValidCell [(i, j) | i <- [0 .. boardSize board - 1], j <- [0 .. boardSize board - 1]]
