-- 15. Crie uma fun¸c˜ao que receba um n´umero n e retorne a fun¸c˜ao totiente de Euler (φ(n)). A fun¸c˜ao totiente
-- de Euler ´e dada pelo n´umero de inteiros positivos r a partir de 1 e menores que n, ou seja 1 <= r < n,
-- que s˜ao coprimos de n. Por exemplo, se n = 10, ent˜ao os coprimos de 10 de 1 at´e 10-1 s˜ao {1, 3, 7, 9} e
-- a fun¸c˜ao deve retornar φ(n) = 4. Leia n do teclado.

mdc :: Int -> Int -> Int
mdc x y = if y == 0 then x else mdc y (mod x y)

coprimos :: Int -> Int -> Bool
coprimos x y = if mdc x y == 1 then True else False

totiente :: Int -> Int 
totiente n = length [x | x <- [1..n-1], coprimos n x]


main :: IO()
main = do
    putStrLn "Digite um número: "
    n <- getLine
    let x = read n
    print ("Totiente de Euler: " ++ show (totiente x))

