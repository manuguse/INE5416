import Data.List (nub, transpose)
import Data.Maybe (isJust, isNothing, fromJust, listToMaybe)
import Control.Applicative ((<|>))

-- Definições de tipos para o tabuleiro e os blocos
type Board = [[Maybe Int]] -- Tabuleiro do puzzle (cada célula pode conter um número ou ser vazia)
type Blocks = [[Int]] -- Blocos do puzzle (cada bloco é identificado por um número)

-- Estrutura do puzzle que contém o tabuleiro, os blocos e o tamanho do tabuleiro
data Puzzle = Puzzle
  { board :: Board -- Tabuleiro
  , blocks :: Blocks -- Blocos
  , size :: Int -- Tamanho do tabuleiro
  } deriving Show -- Permite exibir a estrutura do puzzle

-- Função para inicializar o puzzle com o tabuleiro e os blocos
initializePuzzle :: Board -> Blocks -> Puzzle
initializePuzzle b bs = Puzzle b bs (length b)

-- Funções para manipular o tabuleiro
getBoardPosition :: Board -> Int -> Int -> Maybe Int
getBoardPosition b row col = b !! row !! col -- Obtém o valor de uma célula do tabuleiro (se vazia, retorna Nothing)

setBoardPosition :: Board -> Int -> Int -> Maybe Int -> Board
setBoardPosition b row col value = 
  take row b ++ 
  [take col (b !! row) ++ [value] ++ drop (col + 1) (b !! row)] ++
  drop (row + 1) b

getBlockId :: Blocks -> Int -> Int -> Int
getBlockId bl row col = bl !! row !! col

-- Função auxiliar para obter as células de uma região
getRegionCells :: Blocks -> Int -> [(Int, Int)]
getRegionCells bl regionId = 
    [ (r, c)  -- uma lista de tuplas (r, c) com as coordenadas de cada célula da região
    | r <- [0 .. length bl - 1] -- para cada linha r do tabuleiro de blocos
    , c <- [0 .. length bl - 1] -- para cada coluna c do tabuleiro de blocos
    , getBlockId bl r c == regionId -- se o bloco da célula (r, c) for igual ao id da região
    ]

getAdjacentRegions :: Board -> Blocks -> Int -> [Int]
getAdjacentRegions b bl regionId =
  let regionCells = getRegionCells bl regionId
      neighborRegions = nub $ concatMap (\(r, c) -> 
        [getBlockId bl r' c' | (r', c') <- getOrthogonalNeighbors b r c]) regionCells
  in filter (/= regionId) neighborRegions
  
getRegionSize :: Blocks -> Int -> Int
getRegionSize bl regionId = length $ getRegionCells bl regionId

-- Função auxiliar para obter vizinhos ortogonais (células adjacentes)
getOrthogonalNeighbors :: Board -> Int -> Int -> [(Int, Int)]
getOrthogonalNeighbors b row col = -- retorna uma lista de tuplas (r, c) com as coordenadas dos vizinhos ortogonais
  filter 
    (\(r, c) -> r >= 0 && r < length b && c >= 0 && c < length b) -- filtra as células dentro dos limites do tabuleiro
    [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)] -- vizinhos acima, abaixo, à esquerda e à direita

-- Função de validação para verificar se um número pode ser colocado em uma célula
isValid (Puzzle b bl _) row col num =
  let regionId = getBlockId bl row col
      regionCells = getRegionCells bl regionId
      regionNumbers = map fromJust $ filter isJust [getBoardPosition b r c | (r, c) <- regionCells]
      rowNumbers = nub . map fromJust . filter isJust $ b !! row
      colNumbers = nub . map fromJust . filter isJust $ transpose b !! col
      blockRule = num `notElem` regionNumbers
      verticalBlockRule = all (\(r', _) -> 
        let n = getBoardPosition b r' col
        in isNothing n || (r' < row && fromJust n < num) || (r' > row && fromJust n > num))
        [(r', col) | (r', c') <- regionCells, c' == col]
      neighbors = getOrthogonalNeighbors b row col
      neighborRule = all (\(r, c) -> getBoardPosition b r c /= Just num) neighbors
  in num `notElem` rowNumbers &&
     num `notElem` colNumbers &&
     blockRule &&
     verticalBlockRule &&
     neighborRule



-- Função para resolver o puzzle usando backtracking
solve :: Puzzle -> Maybe Board
solve p@(Puzzle b bl sz)
  | null emptyCells = Just b -- Se não houver células vazias, o puzzle está resolvido
  | otherwise = foldr (\num acc -> acc <|> solve (Puzzle (setBoardPosition b r c (Just num)) bl sz)) Nothing validNumbers
  where
    emptyCells = [(r, c) | r <- [0 .. sz - 1], c <- [0 .. sz - 1], isNothing (getBoardPosition b r c)]
    (r, c) = head emptyCells
    regionId = getBlockId bl r c
    regionSize = getRegionSize bl regionId
    regionNumbers = map fromJust $ filter isJust [getBoardPosition b x y | (x, y) <- getRegionCells bl regionId]
    validNumbers = [num | num <- [1 .. regionSize], num `notElem` regionNumbers]


-- Função para exibir o tabuleiro
printBoard :: Board -> IO ()
printBoard = mapM_ (putStrLn . unwords . map (maybe "." show))

-- Exemplos de tabuleiro (board1)
board1 :: Board
board1 =
  [ [Just 2, Nothing, Nothing, Nothing, Just 1, Nothing]
  , [Nothing, Nothing, Nothing, Just 3, Nothing, Nothing]
  , [Nothing, Just 3, Nothing, Nothing, Just 5, Just 3]
  , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  , [Nothing, Nothing, Just 3, Nothing, Just 4, Just 2]
  , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  ]

blocks1 :: Blocks
blocks1 =
  [ [1, 1, 2, 2, 2, 3]
  , [4, 4, 4, 4, 4, 3]
  , [5, 6, 6, 6, 4, 7]
  , [5, 5, 5, 6, 7, 7]
  , [8, 8, 9, 10, 10, 10]
  , [11, 11, 9, 9, 10, 10]
  ]


board2 :: Board
board2 =
  [ [Nothing, Nothing]
  , [Nothing, Nothing]
  ]

blocks2 :: Blocks
blocks2 =
  [ [1, 1]
  , [2, 2]
  ]

-- Função principal para testar o puzzle
main :: IO ()
main = do
  let puzzle = initializePuzzle board2 blocks2
  putStrLn "Tabuleiro inicial:"
  printBoard (board puzzle)
  case solve puzzle of
    Just solution -> do
      putStrLn "\nSolução encontrada:"
      printBoard solution
    Nothing -> putStrLn "\nNão foi possível encontrar uma solução."