-- 14. Crie uma fun¸c˜ao que receba dois n´umeros x e y e determine se eles s˜ao coprimos. Dois n´umeros s˜ao ditos
-- coprimos se o m´aximo divisor comum entre eles ´e 1. Leia x e y do teclado.

mdc :: Int -> Int -> Int
mdc x y = if y == 0 then x else mdc y (mod x y)

coprimos :: Int -> Int -> Bool
coprimos x y = if mdc x y == 1 then True else False

main :: IO()
main = do
    putStrLn "Digite os dois números separados por espaço: "
    v <- getLine
    let [x, y] = map read (words v)
    print ("Os numeros sao coprimos? " ++ show (coprimos x y))