-- 17. Crie uma fun¸c˜ao que receba um n´umero n e retorne se o mesmo ´e primo. Leia n do teclado.


primo :: Int -> Bool
primo n = if length [x | x <- [1..n], n `mod` x == 0] == 2 then True else False

main :: IO()
main = do
    putStrLn "Digite um número: "
    n <- getLine
    let x = read n
    print ("O número é primo? " ++ show (primo x))
