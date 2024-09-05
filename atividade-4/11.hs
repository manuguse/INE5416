-- 11. Crie uma fun¸c˜ao que receba dois n´umeros x e y e retorne o m´aximo divisor comum (DICA: pesquise sobre
-- o Algoritmo de Euclides). Leia x e y do teclado.

euclides :: Int -> Int -> Int
euclides x y = if y == 0 then x else euclides y (mod x y)

main :: IO ()
main = do
    putStrLn "Digite o primeiro número: "
    x <- getLine
    putStrLn "Digite o segundo número: "
    y <- getLine
    print (euclides (read x) (read y))