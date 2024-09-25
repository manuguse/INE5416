-- 4. Crie uma fun¸c˜ao com assinatura diferencaMaiorMenor :: [Int] -> Int, a qual recebe uma lista de
-- Int e retorna a diferen¸ca entre o maior e o menor elemento da lista. Retorne 0 caso a lista for vazia. N˜ao
-- utilize nenhuma fun¸c˜ao pronta do Haskell para realizar esta tarefa

maior :: [Int] -> Int
maior [] = 0
maior [x] = x
maior (x:xs) = if x > maior xs then x else maior xs

menor :: [Int] -> Int
menor [] = 0 
menor [x] = x 
menor (x:xs) = if x < menor xs then x else menor xs 

diferencaMaiorMenor :: [Int] -> Int
diferencaMaiorMenor [] = 0
diferencaMaiorMenor xs = maior xs - menor xs

main :: IO ()
main = do
    print (diferencaMaiorMenor [1,4,-2,3,5])
    print (diferencaMaiorMenor [1,2,3,4,5])
    print (diferencaMaiorMenor [50,-4,3,2,1])
