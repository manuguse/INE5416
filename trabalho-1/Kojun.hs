module Kojun (solve) where

-- requisitos para o kojun:
---- 1. não pode haver números repetidos em um bloco
---- 2. não pode haver números repetidos em células adjacentes
---- 3. dentro de um bloco, não pode haver um menor em cima de um maior

import Data.Maybe (isNothing, fromMaybe)
import Data.List (transpose, (\\))

type Board = [[Maybe Int]]

solve :: Board -> Board -> Maybe Board
solve values groups = do
    return values
