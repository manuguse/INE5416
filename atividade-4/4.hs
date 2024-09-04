-- 4. Crie uma fun¸c˜ao que receba dois valores booleanos (x, y) retorne o resultado do “ou exclusivo” (XOR)
-- sobre eles. A fun¸c˜ao apenas deve usar os operadores &&, || e not. Leia os valores x e y do teclado.

xor :: Bool -> Bool -> Bool
xor x y = (x || y) && not (x && y)

main :: IO ()
main = do
    putStrLn "Digite o primeiro valor booleano (True/False): "
    x <- getLine
    putStrLn "Digite o segundo valor booleano (True/False): "
    y <- getLine
    let boolX = read x :: Bool
    let boolY = read y :: Bool
    print (xor boolX boolY)