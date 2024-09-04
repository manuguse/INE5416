-- 8. Crie uma fun¸c˜ao que resolva uma equa¸c˜ao de segundo grau da forma ax2 + bx + c utilizando a f´ormula
-- de Bhaskara. Leia os coeficientes a, b e c do teclado

bhaskara :: Float -> Float -> Float -> (Float, Float)
bhaskara a b c = 
    let delta = b^2 - 4 * a * c
        x1 = (-b + sqrt delta) / (2 * a)
        x2 = (-b - sqrt delta) / (2 * a)
    in (x1, x2)

main :: IO ()
main = do
    putStrLn "Digite o valor de a: "
    a <- getLine
    putStrLn "Digite o valor de b: "
    b <- getLine
    putStrLn "Digite o valor de c: "
    c <- getLine
    print (bhaskara (read a) (read b) (read c))