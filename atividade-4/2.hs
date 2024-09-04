-- Crie uma fun¸c˜ao que receba um n´umero x, negativo ou positivo, e retorne seu valor absoluto. Leia x do teclado

absoluto :: Int -> Int
-- absoluto x = abs x
absoluto x = if x < 0 then -x else x

main :: IO ()
main = do
    putStrLn "Digite um número: "
    x <- getLine
    print (absoluto (read x))