-- 13. Crie uma fun¸c˜ao com assinatura apagarEnquanto :: (t -> Bool) -> [t] -> [t], a qual recebe uma
-- fun¸c˜ao como parˆametro e uma lista, e retorna uma lista. Esta fun¸c˜ao deve aplicar a fun¸c˜ao passada como
-- parˆametro a cada elemento da lista, at´e que algum elemento da lista retorne False na aplica¸c˜ao da fun¸c˜ao.
-- Os elementos da lista resultante s˜ao ent˜ao todos os elementos a partir do elemento em que a fun¸c˜ao passada
-- como parˆametro retornou False. Por exemplo a chamada (apagarEnquanto par [2,4,1,2,3,4,5])
-- deve retornar [1,2,3,4,5], visto que ao testar o elemento 1, a fun¸c˜ao par retorna False. N˜ao utilize
-- nenhuma fun¸c˜ao pronta to Haskell para esta tarefa

apagarEnquanto :: (t -> Bool) -> [t] -> [t]
apagarEnquanto _ [] = [] -- caso base (lista vazia)
apagarEnquanto f (x:xs) = if f x then apagarEnquanto f xs else x:xs

main :: IO()
main = do
    print (apagarEnquanto (\x -> x `mod` 2 == 0) [2,4,1,2,3,4,5]) -- [1,2,3,4,5]
    print (apagarEnquanto (\x -> x `mod` 2 == 0) [2,4,6,8,10]) -- []
    print (apagarEnquanto (\x -> x `mod` 2 == 0) [1,3,5,7,9]) -- [1,3,5,7,9]
    print (apagarEnquanto (\x -> x `mod` 2 == 0) [1,2,3,4,5,6,7,8,9,10]) -- [1,2,3,4,5,6,7,8,9,10]
    print (apagarEnquanto (\x -> x `mod` 2 == 0) [1,2,3,4,5,6,7,8,9,10,11]) -- [1,2,3,4,5,6,7,8,9,10,11]
    print (apagarEnquanto (\x -> x `mod` 2 == 0) [2,4,6,8,10,1,3,5,7,9]) -- [1,3,5,7,9]
    print (apagarEnquanto (\x -> x `mod` 2 == 0) [1,3,5,7,9,2,4,6,8,10]) -- [1,3,5,7,9,2,4,6,8,10]
    print (apagarEnquanto (\x -> x `mod` 2 == 0) [1,3,5,7,9,2,4,6,8,10,11]) -- [1,3,5,7,9,2,4,6,8,10,11]
    print (apagarEnquanto (\x -> x `mod` 2 == 0) [2,4,6,8,10,1,3,5,7,9,11]) -- [1,3,5