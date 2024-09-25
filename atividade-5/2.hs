-- 2. Crie uma fun¸c˜ao com assinatura media :: [Int] -> Float, a qual recebe uma lista de Int e retorna
-- a m´edia de todos os elementos da lista. Retorne 0 caso a lista for vazia. N˜ao utilize nenhuma fun¸c˜ao
-- pronta do Haskell para realizar esta tarefa. DICA: utilize a fun¸c˜ao fromIntegral para converter um tipo
-- inteiro para um tipo compat´ıvel com o operador de divis˜ao 
-- N˜ao utilize nenhuma fun¸c˜ao pronta do Haskell para realizar esta tarefa. 

soma :: [Int] -> Int
soma [] = 0 -- caso base (lista vazia)
soma (x:xs) = x + soma xs -- caso recursivo (soma o primeiro elemento com a soma do restante da lista)

media :: [Int] -> Float
media [] = 0 -- caso base (lista vazia)
media xs = fromIntegral (soma xs) / fromIntegral (length xs) -- caso recursivo (soma todos os elementos e divide pelo tamanho da lista)

main :: IO ()
main = do
    print (media [1,2,3,4,5])