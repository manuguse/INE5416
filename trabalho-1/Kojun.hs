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
    -- emptyCells é uma lista de tuplas (row, column) tal que board na posição (row, column) é Nothing
    let emptyCells = [(row, column) | row <- [0..length board - 1], column <- [0..length (board !! row) - 1], isNothing (board !! row !! column)]
    in debugTrace ("Células vazias encontradas: " ++ show emptyCells) $
       if null emptyCells then Nothing else Just (head emptyCells) -- retorna a primeira célula vazia ou Nothing se não tiver mais vazias

-- tenta preencher uma célula vazia com valores possíveis (possibleValues) e verifica se o tabuleiro é válido.
tryFillCell :: Board -> Board -> Maybe (Int, Int) -> Maybe Board
tryFillCell _ _ Nothing = debugTrace "Nenhuma célula vazia encontrada" Nothing
tryFillCell board blocks (Just (row, column)) = debugTrace ("Tentando preencher célula (" ++ show row ++ ", " ++ show column ++ ")") $
    let maxValue = maxValueInBlock blocks row column -- calcula o valor máximo permitido para a célula
        possibleValues = [1..maxValue] -- os valores possíveis vão de 1 até o valor máximo (tamanho do bloco)
        -- para pegar os tabuleiros válidos, mapeia a função updateBoard para cada valor possível e verifica se o tabuleiro é válido
        -- o valid boards vai ser uma lista de tabuleiros tal que cada tabuleiro é o tabuleiro original com a célula (row, column) 
            -- preenchida com um valor v tal que v é um valor possível
        validBoards = [updateBoard board row column (Just v) | v <- possibleValues, verify (updateBoard board row column (Just v)) blocks]
    in tryBoards validBoards blocks -- tenta resolver o tabuleiro para cada um dos tabuleiros válidos

-- calcula o valor máximo permitido para uma célula com base no bloco (será o próprio taamanho do bloco)
maxValueInBlock :: Board -> Int -> Int -> Int
maxValueInBlock blocks row column = debugTrace ("Calculando valor máximo para célula (" ++ show row ++ ", " ++ show column ++ ")") $
    let blockId = fromMaybe (-1) (blocks !! row !! column) -- pega o id do bloco da célula
        -- as células do bloco são as células (i, j) tal que blocks na posição (i, j) é igual ao id do bloco
        blockCells = [(i, j) | i <- [0..length blocks - 1], j <- [0..length (blocks !! i) - 1], blocks !! i !! j == Just blockId]
    in length blockCells -- o valor máximo é o tamanho do bloco

-- tenta resolver o tabuleiro para cada uma das possíveis configurações válidas
tryBoards :: [Board] -> Board -> Maybe Board
tryBoards [] _ = debugTrace "Nenhum tabuleiro válido encontrado" Nothing -- se não houver tabuleiros válidos, retorna Nothing
tryBoards (b:bs) blocks = debugTrace ("Tentando resolver tabuleiro:\n" ++ show b) $ -- tenta resolver o tabuleiro
    case solveBoard b blocks of -- tenta resolver o tabuleiro, de forma recursiva (chamando solveBoard)
        Just solved -> debugTrace "Tabuleiro resolvido" (Just solved) -- se conseguir resolver, retorna o tabuleiro resolvido
        Nothing -> tryBoards bs blocks -- se não conseguir resolver, tenta o próximo tabuleiro

-- atualiza o tabuleiro com um novo valor em uma célula específica
updateBoard :: Board -> Int -> Int -> Maybe Int -> Board
updateBoard board row column val =
    -- divide o tabuleiro em 3 partes: antes da linha, linha que vamos usar e depois da linha
    let (beforeRow, targetRow:afterRow) = splitAt row board
        -- divide a linha que vamos usar em 3 partes: antes da coluna, célula que vamos usar e depois da coluna
        (beforeCol, _:afterCol) = splitAt column targetRow
        newRow = beforeCol ++ [val] ++ afterCol  -- atualiza a célula
    in beforeRow ++ [newRow] ++ afterRow -- retorna o tabuleiro atualizado

-- verifica se o tabuleiro é válido de acordo com as regras do jogo
verify :: Board -> Board -> Bool
verify board blocks = debugTrace ("Verificando tabuleiro:\n" ++ show board)
    -- faz um and entre as 3 verificações necessárias para um tabuleiro ser válido
    (noRepeatedNumberInBlock board blocks && noRepeatedNumberInAdjacentCells board && noSmallerNumberOnTopOfBiggerNumber board blocks)

-- verifica se todas as células do tabuleiro estão preenchidas
allFilled :: Board -> Bool
allFilled board = debugTrace ("Verificando se todas as células estão preenchidas:\n" ++ show board) $
    all (all isJust) board -- verifica se todas as células são Just

-- verifica se não há números repetidos em um bloco
noRepeatedNumberInBlock :: Board -> Board -> Bool
noRepeatedNumberInBlock board blocks = debugTrace ("\nNão há números repetidos: " ++ show result ++ "\n") result
  where
    uniqueBlocks = uniqueElements blocks -- pega as coordenadas únicas de cada bloco
    -- pega os valores na posição (row, column) de cada bloco de forma que o valor não seja Nothing
    noRepeatsInBlock block = let values = [board !! row !! column | (row, column) <- block, isJust (board !! row !! column)]
                             in debugTrace ("Valores do bloco: " ++ show values) $
                                length values == length (nub values)
    -- verifica se não há números repetidos em cada bloco
    result = all noRepeatsInBlock uniqueBlocks

-- retorna uma lista de coordenadas únicas para cada bloco
uniqueElements :: Eq a => [[a]] -> [[(Int, Int)]]
uniqueElements xss = debugTrace "\nProcurando elementos únicos\n" $
    map (\x -> [(row, column) | row <- [0..length xss - 1], 
                                -- cria uma lista de coordenadas para cada valor 'x' tal que
                                column <- [0..length (xss !! row) - 1], 
                                -- itera sobre todas as colunas de cada linha, pertencendo ao bloco 'x'
                                xss !! row !! column == x]) 
        -- nub remove duplicatas da lista 'concat xss', resultando em uma lista de elementos únicos de 'xss'
        (nub (concat xss))

-- verifica se não há números repetidos em células adjacentes
noRepeatedNumberInAdjacentCells :: Board -> Bool
noRepeatedNumberInAdjacentCells board = debugTrace ("\nNão há celulas adjacentes com valores iguais: " ++ show result ++ "\n") result
  where
    noRepeatsInAdjacentCells (row, column) =
        let cellValue = board !! row !! column -- pega o valor da célula em (row, column)
            -- pega as posições adjacentes à célula (row, column) 
            adjacentPositions = [(row-1, column), (row+1, column), (row, column-1), (row, column+1)]
            -- vai pegar os valores se estiver dentro dos limites do tabuleiro
            adjacentValues = [board !! row' !! column' | (row', column') <- adjacentPositions, row' >= 0, row' < length board, column' >= 0, column' < length (board !! row')]
        in debugTrace ("Célula (" ++ show row ++ ", " ++ show column ++ ") valor: " ++ show cellValue ++ ", Valores adjacentes: " ++ show adjacentValues) $
           isNothing cellValue || notElem cellValue adjacentValues
    -- verifica se não há números repetidos em células adjacentes para todas as células do tabuleiro
    result = all noRepeatsInAdjacentCells [(row, column) | row <- [0..length board - 1], column <- [0..length (board !! row) - 1]]

-- verifica se não há números menores em cima de números maiores no mesmo bloco
noSmallerNumberOnTopOfBiggerNumber :: Board -> Board -> Bool
noSmallerNumberOnTopOfBiggerNumber board blocks =
    debugTrace ("\nVerificando números menores em cima de maiores no mesmo bloco: " ++ show result) result
  where
    isValidCell (i, j) = -- verifica se a célula (i, j) é válida nesse aspecto
        let currentValue = board !! i !! j -- pega o valor da célula (i, j)
        -- pega o valor de cima se houver valor e pertencer ao mesmo bloco
            aboveValue = if i > 0 && (blocks !! i !! j == blocks !! (i - 1) !! j)
                         then board !! (i - 1) !! j
                         else Nothing
        in debugTrace ("Célula (" ++ show i ++ ", " ++ show j ++ ") valor: " ++ show currentValue ++ ", Acima: " ++ show aboveValue) $
           case (currentValue, aboveValue) of
            -- se houver o valor, verifica se o valor de cima é maior ou igual ao valor atual
               (Just curr, Just above) -> curr <= above 
               -- se não, a célula é declarada válida já que não precisamos nos preocupar com o valor de cima
               _ -> True

    -- verifica se não há números menores em cima de números maiores para todas as células do tabuleiro
    result = all isValidCell [(i, j) | i <- [0 .. boardSize board - 1], j <- [0 .. boardSize board - 1]]
