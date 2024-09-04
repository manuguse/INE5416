-- 1. Crie uma função que receba dois números x e y e retorne x y. Leia x e y do teclado.

quad :: Int -> Int -> Int
quad x y = x ^ y

main :: IO ()
main = do
    putStrLn "Digite o primeiro número: "
    x <- getLine
    putStrLn "Digite o segundo número: "
    y <- getLine
    print (quad (read x) (read y))