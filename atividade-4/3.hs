-- 3. Crie uma fun¸c˜ao que receba a base e a altura de um triˆangulo e calcule a ´area do mesmo. Leia a base e a
-- altura do teclado.

calculaArea :: Float -> Float -> Float
calculaArea base altura = (base * altura) / 2

main :: IO ()
main = do
    putStrLn "Digite a base do triângulo: "
    base <- getLine
    putStrLn "Digite a altura do triângulo: "
    altura <- getLine
    print (calculaArea (read base) (read altura))