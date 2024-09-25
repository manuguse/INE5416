-- 10. Crie uma fun¸c˜ao com assinatura filtrar :: (t -> Bool) -> [t] -> [t], a qual recebe uma fun¸c˜ao,
-- uma lista e retorna uma nova lista. Esta fun¸c˜ao aplica a fun¸c˜ao recebida como parˆametro sobre cada
-- elemento da lista e caso o retorno da fun¸c˜ao for verdadeiro, ent˜ao o elemento far´a parte da nova lista, caso
-- contr´ario n˜ao. Para esta tarefa, utilize o conceito de list comprehension.

filtrar :: (t -> Bool) -> [t] -> [t]
filtrar _ [] = [] -- caso base (lista vazia)
filtrar f (x:xs) = [x | x <- (x:xs), f x]

main :: IO()
main = do
    print (filtrar (\x -> x > 3) [1,2,3,4,5]) -- [4,5]
    print (filtrar (\x -> x < 3) [1,2,3,4,5]) -- [1,2]
    print (filtrar (\x -> x == 3) [1,2,3,4,5]) -- [3]
    print (filtrar (\x -> x == 6) [1,2,3,4,5]) -- []