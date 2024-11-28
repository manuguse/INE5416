-- requisitos para o kojun:
---- 1. não pode haver números repetidos em um bloco
---- 2. não pode haver números repetidos em células adjacentes
---- 3. dentro de um bloco, não pode haver um menor em cima de um maior

module Kojun (solve) where

import Data.Maybe (isNothing, fromMaybe)
import Data.List (transpose, (\\))

type Table = [[Maybe Int]]
type Matrix a = [[a]]
type Choices = [Int]

-- Conta quantas células pertencem a um bloco específico
groupSize :: Int -> Table -> Int
groupSize id groups = length $ filter (== Just id) (concat groups)

-- Retorna os valores que já estão presentes em um bloco
getValuesInGroup :: Table -> Table -> Int -> [Int]
getValuesInGroup values groups id =
    [fromMaybe 0 val | (val, groupId) <- zip (concat values) (concat groups), groupId == Just id, not (isNothing val)]

-- Remove escolhas inválidas para células com múltiplas opções
minus :: Choices -> Choices -> Choices
xs `minus` ys = if length xs == 1 then xs else xs \\ ys

-- Gera as escolhas para cada célula
choices :: Table -> Table -> Matrix Choices
choices values groups =
    [[if isNothing val
        then [1..groupSize (fromMaybe 0 group) groups] `minus` getValuesInGroup values groups (fromMaybe 0 group)
        else [fromMaybe 0 val]
      | (val, group) <- zip rowVal rowGroup]
     | (rowVal, rowGroup) <- zip values groups]

-- Reduz as escolhas de acordo com regras do jogo
reduceChoices :: Matrix Choices -> Table -> Matrix Choices
reduceChoices values groups = map reduceRow values
  where
    reduceRow row = [xs `minus` singles | xs <- row]
      where
        singles = concat $ filter ((== 1) . length) row

-- Função principal que resolve o Kojun usando backtracking
solve :: Table -> Table -> Maybe (Matrix Int)
solve values groups =
    let initialChoices = choices values groups
        reducedChoices = reduceChoices initialChoices groups
    in searchForSolution reducedChoices groups

-- Função de busca por solução usando backtracking
searchForSolution :: Matrix Choices -> Table -> Maybe (Matrix Int)
searchForSolution values groups
    | notPossible values = Nothing
    | all (all ((== 1) . length)) values = Just $ map (map head) values
    | otherwise = case concatMap (\val -> searchForSolution (reduceChoices val groups) groups) (expandChoices values) of
        [] -> Nothing
        (sol:_) -> Just sol

-- Verifica se o estado atual não permite solução
notPossible :: Matrix Choices -> Bool
notPossible = any (any null)

-- Expande escolhas para exploração no backtracking
expandChoices :: Matrix Choices -> [Matrix Choices]
expandChoices m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
  where
    (rows1, row : rows2) = break (any ((> 1) . length)) m
    (row1, cs : row2) = break ((> 1) . length) row
