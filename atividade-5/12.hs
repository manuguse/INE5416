-- 12. Crie uma fun¸c˜ao com assinatura apagar :: Int -> [t] -> [t], a qual recebe um n´umero de elementos,
-- uma lista, e retorna uma lista. Esta fun¸c˜ao deve remover da lista os n primeiros elementos fornecidos
-- como parˆametro. Por exemplo, a chamada (apagar 3 [1,2,3,4,5]) deve retornar [4,5]. N˜ao utilize
-- nenhuma fun¸c˜ao pronta to Haskell para esta tarefa

apagar :: Int -> [t] -> [t]
apagar 0 xs = xs -- caso base (n = 0)
apagar _ [] = [] -- caso base (lista vazia)
apagar n (x:xs) = apagar (n-1) xs -- se n > 0, chama a funcao recursivamente com n-1

main :: IO()
main = do
    print (apagar 3 [1,2,3,4,5]) -- [4,5]
    print (apagar 5 [1,2,3,4,5]) -- []