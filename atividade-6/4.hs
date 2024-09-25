-- 4. Modifique o arquivo arvore.hs (disponível no Moodle) de forma a adicionar novas operações à nossa
-- árvore:
-- A: Crie uma função com a seguinte assinatura: ocorrenciasElemento :: Arvore -> Int -> Int, a
-- qual recebe um número e deve retornar a quantidade de ocorrências dele na árvore.
-- B: Crie uma função com a seguinte assinatura: maioresQueElemento :: Arvore -> Int -> Int, a
-- qual recebe um número e deve retornar a quantidade de números maiores que ele na árvore.
-- C: Crie uma função com a seguinte assinatura: mediaElementos :: Arvore -> Float, a qual deve
-- retornar a média dos números na árvore. DICA: utilize a função fromIntegral para converter um
-- tipo inteiro para um tipo compatível com o operador de divisão /.
-- D: Crie uma função com a seguinte assinatura: quantidade :: Arvore -> Int, a qual deve retornar
-- a quantidade de elementos na árvore.
-- E: Crie uma função com a seguinte assinatura: elementos :: Arvore -> [Int], a qual deve retornar
-- uma lista com todos os elementos na árvore.

data Arvore = Null | No Int Arvore Arvore

ocorrenciasElemento :: Arvore -> Int -> Int
ocorrenciasElemento Null _ = 0
ocorrenciasElemento (No n esq dir) x = 
    (if n == x then 1 else 0) + (ocorrenciasElemento esq x) + (ocorrenciasElemento dir x)

maioresQueElemento :: Arvore -> Int -> Int
maioresQueElemento Null _ = 0
maioresQueElemento (No n esq dir) x = 
    (if n > x then 1 else 0) + (maioresQueElemento esq x) + (maioresQueElemento dir x)

quantidade :: Arvore -> Int
quantidade Null = 0
quantidade (No _ esq dir) = 1 + (quantidade esq) + (quantidade dir)

mediaElementos :: Arvore -> Float
mediaElementos Null = 0
mediaElementos arvore = fromIntegral (somaElementos arvore) / fromIntegral (quantidade arvore)

elementos :: Arvore -> [Int]
elementos Null = []
elementos (No n esq dir) = n : (elementos esq) ++ (elementos dir)

minhaArvore :: Arvore
minhaArvore = No 52 (No 32 (No 12 Null Null) (No 35 Null Null)) (No 56 (No 55 Null Null) (No 64 Null Null))

somaElementos :: Arvore -> Int
somaElementos Null = 0
somaElementos (No n esq dir) = n + (somaElementos esq) + (somaElementos dir)

buscaElemento :: Arvore -> Int -> Bool
buscaElemento Null _ = False
buscaElemento (No n esq dir) x 
    | (n == x) = True                           
    | otherwise = (buscaElemento esq x) || (buscaElemento dir x)

limiteSup :: Int
limiteSup = 1000 --Define um limite superior para o maior número

minimo :: Int -> Int -> Int
minimo x y | (x < y) = x
           | otherwise = y

minimoElemento :: Arvore -> Int
minimoElemento Null = limiteSup 
minimoElemento (No n esq dir) = 
    minimo n (minimo (minimoElemento esq) (minimoElemento dir))
                               
main = do putStrLn (show (somaElementos minhaArvore))
          putStrLn (show (buscaElemento minhaArvore 30))
          putStrLn (show (buscaElemento minhaArvore 55))
          putStrLn (show (minimoElemento minhaArvore))
          putStrLn (show (ocorrenciasElemento minhaArvore 32))
          putStrLn (show (maioresQueElemento minhaArvore 32))
          putStrLn (show (mediaElementos minhaArvore))
          putStrLn (show (quantidade minhaArvore))
          putStrLn (show (elementos minhaArvore))
