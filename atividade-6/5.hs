-- 5. Pesquise o que é o newtype. Qual é a diferença dele para o type e para o data? Faça um pequeno
-- exemplo de aplicação e explique seu funcionamento.

-- newtype é uma palavra-chave que define um novo tipo de dado, mas que possui uma única representação interna.
-- a diferença entre newtype e data é que newtype é mais eficiente, já que ele é representado por um único valor, 
-- enquanto data é representado por um conjunto de valores.

-- Exemplo de aplicação:

newtype Nome = Nome String deriving Show

main :: IO()
main = do
    print (Nome "Joao") -- Nome "João"