-- 6. Crie uma fun¸c˜ao com assinatura ocorrencias :: [Int] -> Int -> Int, a qual recebe uma lista de
-- Int e um Int e retorna o n´umero de vezes em que o elemento est´a presente na lista. N˜ao utilize nenhuma
-- fun¸c˜ao pronta do Haskell para realizar esta tarefa.

ocorrencias :: [Int] -> Int -> Int
ocorrencias [] _ = 0 -- caso base (lista vazia)
ocorrencias (x:xs) n = if x == n then 1 + ocorrencias xs n else ocorrencias xs n

main :: IO()
main = do
    print (ocorrencias [1,2,3,4,5] 3) -- 1
    print (ocorrencias [1,2,3,4,5] 6) -- 0
    print (ocorrencias [1,2,3,4,5,3,3,3,3] 3) -- 5