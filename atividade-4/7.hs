-- 7. Crie uma fun¸c˜ao que compute o n-´esimo n´umero de Fibonacci. Leia n do teclado.

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

main :: IO ()
main = do
    putStrLn "Digite o valor de n: "
    n <- getLine
    print (fibonacci (read n))
