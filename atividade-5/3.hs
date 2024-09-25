-- 3. Crie uma fun¸c˜ao com assinatura menor :: [Int] -> Int, a qual recebe uma lista de Int e retorna o
-- menor elemento da lista. Retorne 0 caso a lista for vazia. N˜ao utilize nenhuma fun¸c˜ao pronta do Haskell
-- para realizar esta tarefa.

menor :: [Int] -> Int
menor [] = 0  -- caso base (lista vazia)
menor [x] = x -- caso base (lista com um elemento)
menor (x:xs) = if x < menor xs then x else menor xs -- caso recursivo (compara o primeiro elemento com o menor dos restantes)

main :: IO ()
main = do
    print (menor [1,4,-2,3,5])
    print (menor [1,2,3,4,5])
    print (menor [5,4,3,2,1])