-- 1. Crie uma fun¸c˜ao com assinatura soma :: [Int] -> Int, a qual recebe uma lista de Int e retorna a soma
-- de todos os elementos da lista. Retorne 0 caso a lista for vazia. N˜ao utilize nenhuma fun¸c˜ao pronta do
-- Haskell para realizar esta tarefa.

soma :: [Int] -> Int
soma [] = 0 -- caso base (lista vazia)
soma (x:xs) = x + soma xs -- caso recursivo (soma o primeiro elemento com a soma do restante da lista)

main :: IO ()
main = do
    print (soma [1, 2, 3, 4, 5])
    print (soma [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
    print (soma [])