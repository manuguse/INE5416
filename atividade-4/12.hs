-- 12. Crie uma fun¸c˜ao que receba trˆes n´umeros x, y e z e retorne o m´aximo divisor comum (DICA: apenas
-- modifique o algoritmo anterior). Leia x, y e z do teclado.

mdc :: Int -> Int -> Int -> Int
mdc x y z = euclides (euclides x y) z where
    euclides :: Int -> Int -> Int
    euclides x y = if y == 0 then x else euclides y (mod x y)

main :: IO ()
main = do
    putStrLn "Digite os tres números separados por espaço: "
    v <- getLine
    let [x, y, z] = map read (words v)
    print ("Maximo divisor comum: " ++ show (mdc x y z))