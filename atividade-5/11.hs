-- 11. Crie uma fun¸c˜ao com assinatura primeiros :: Int -> [t] -> [t], a qual recebe um n´umero de elementos, uma lista, e retorna uma lista. Esta fun¸c˜ao deve retornar uma lista com os n primeiros elementos
-- informados no primeiro parˆametro. N˜ao utilize nenhuma fun¸c˜ao pronta to Haskell para esta tarefa.

primeiros :: Int -> [t] -> [t]
primeiros 0 _ = [] -- caso base (n = 0)
primeiros _ [] = [] -- caso base (lista vazia)
primeiros n (x:xs) = x : primeiros (n-1) xs -- se n > 0, adiciona o elemento x na lista e chama a funcao recursivamente com n-1

main :: IO()
main = do 
    print (primeiros 3 [1,2,3,4,5]) -- [1,2,3]
    print (primeiros 5 [1,2,3,4,5]) -- [1,2,3,4,5]
    print (primeiros 0 [1,2,3,4,5]) -- []
    print (primeiros 3 [1,2]) -- [1,2]
    print (primeiros 3 ([] :: [Int])) -- []
