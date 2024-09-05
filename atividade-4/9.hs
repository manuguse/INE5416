-- 9. Crie uma fun¸c˜ao que dados dois pontos no espa¸co 3D, (x1, y1, z1) e (x2, y2, z2), compute a distˆancia
-- entre eles. Leia as posi¸c˜oes dos pontos do teclado.

calculaDistancia :: Float -> Float -> Float -> Float -> Float -> Float -> Float
calculaDistancia x1 y1 z1 x2 y2 z2 = sqrt ((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2)

main :: IO ()
main = do
    putStrLn "Digite o valor de x1, y1 e z1 (separados por espaço): "
    p1 <- getLine
    putStrLn "Digite o valor de x2, y2 e z2 (separados por espaço): "
    p2 <- getLine
    let [x1, y1, z1] = map read (words p1)
    let [x2, y2, z2] = map read (words p2)
    print (calculaDistancia x1 y1 z1 x2 y2 z2)
