-- 16. Crie uma fun¸c˜ao que receba dois n´umeros x e y e retorne se x ´e divis´ıvel por y. Leia x e y do teclado.

divisivel :: Int -> Int -> Bool
divisivel x y = if x `mod` y == 0 then True else False

main :: IO()
main = do
    putStrLn "Digite dois números separados por espaço: "
    v <- getLine
    let [x, y] = map read (words v)
    print ("x é divisível por y? " ++ divisivel x y)