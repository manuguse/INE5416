-- 6. Crie uma fun¸c˜ao que receba trˆes inteiros x, y e z e retorne se havendo varetas com esses valores em
-- comprimento pode-se construir um triˆangulo. Exemplo, com varetas de comprimento 4, 8 e 9 posso
-- construir um triˆangulo, por´em com varetas de comprimento 10, 5 e 4 n˜ao posso construir um triˆangulo.
-- Leia x, y e z do teclado.

verificaTriangulo :: Int -> Int -> Int -> String
verificaTriangulo x y z = 
    if x + y > z && x + z > y && y + z > x then 
        "eh possivel construir um triangulo"
    else 
        "nao eh possível construir um triangulo"

main :: IO ()
main = do
    putStrLn "Digite o comprimento da primeira vareta: "
    x <- getLine
    putStrLn "Digite o comprimento da segunda vareta: "
    y <- getLine
    putStrLn "Digite o comprimento da terceira vareta: "
    z <- getLine
    print (verificaTriangulo (read x) (read y) (read z))