-- 5. Crie uma fun¸c˜ao com assinatura busca :: [Int] -> Int -> Bool, a qual recebe uma lista de Int e um
-- Int e retorna se o elemento passado como parˆametro encontra-se na lista ou n˜ao. N˜ao utilize nenhuma
-- fun¸c˜ao pronta do Haskell para realizar esta tarefa

busca :: [Int] -> Int -> Bool
busca [] _ = False -- caso base (lista vazia)
busca (x:xs) n = if x == n then True else busca xs n -- caso recursivo (compara o primeiro elemento com o número e chama a função com o restante da lista)

main :: IO()
main = do
    print (busca [1,2,3,4,5] 3) -- True
    print (busca [1,2,3,4,5] 6) -- False