-- 13. Crie uma fun¸c˜ao que receba dois n´umeros x e y e retorne o m´ınimo m´ultiplo comum (DICA: use a fun¸c˜ao
-- do m´aximo divisor comum j´a criada). Leia x e y do teclado

mdc :: Int -> Int -> Int
mdc x y = if y == 0 then x else mdc y (mod x y)

mmc :: Int -> Int -> Int
mmc x y = (x * y) `div` (mdc x y) 

main :: IO ()
main = do
    putStrLn "Digite os dois números separados por espaço: "
    v <- getLine
    let [x, y] = map read (words v)
    print ("Minimo multiplo comum: " ++ show (mmc x y))