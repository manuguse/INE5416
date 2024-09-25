-- 11. Crie uma fun¸c˜ao que receba dois n´umeros x e y e retorne o m´aximo divisor comum (DICA: pesquise sobre
-- o Algoritmo de Euclides). Leia x e y do teclado.

euclides :: Int -> Int -> Int
euclides x y = if y == 0 then x else euclides y (mod x y)

main :: IO ()
main = do
    putStrLn "Digite os dois números separados por espaço: "
    v <- getLine
    let [x, y] = map read (words v)
    print ("Maximo divisor comum: " ++ show (euclides x y))