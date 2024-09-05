-- 10. Crie uma fun¸c˜ao que receba 3 valores num´ericos (a, b, c) e retorne o maior deles. N˜ao utilize nenhuma
-- forma de ordena¸c˜ao. Leia os valores a, b, c do teclado.

maior :: Float -> Float -> Float -> Float
maior a b c = 
    if a > b && a > c then 
        a
    else if b > a && b > c then
        b
    else 
        c

main :: IO ()
main = do
    putStrLn "Digite o valor de a, b e c separados por espaço: "
    v <- getLine
    let [a, b, c] = map read (words v)
    print (ordena a b c)1