-- 8. Crie uma fun¸c˜ao com assinatura inverte :: [t] -> [t], a qual recebe uma lista como parˆametro e
-- deve retornar a mesma invertida. N˜ao utilize nenhuma fun¸c˜ao pronta do Haskell para realizar esta tarefa.

inverte :: [t] -> [t]
inverte [] = [] -- caso base (lista vazia)
inverte (x:xs) = inverte xs ++ [x]

main :: IO()
main = do
    print(inverte [1,2,3,4,5]) -- [5,4,3,2,1]
    print(inverte [1,2,3,4,5,6,7,8,9,10]) -- [10,9,8,7,6,5,4,3,2,1]