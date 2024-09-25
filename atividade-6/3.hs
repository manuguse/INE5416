-- 3. Crie um novo tipo Ponto, usando data, que pode ser um ponto 2D ou um ponto 3D. Depois, crie uma
-- fun¸c˜ao que receba dois pontos (necessariamente ambos sendo 2D ou ambos sendo 3D), e retorne a distˆancia
-- entre eles.

type Ponto2D = (Float, Float)
type Ponto3D = (Float, Float, Float)

distancia :: (Ponto2D, Ponto2D) -> Float
distancia ((x1, y1), (x2, y2)) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

distancia3D :: (Ponto3D, Ponto3D) -> Float
distancia3D ((x1, y1, z1), (x2, y2, z2)) = sqrt ((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2)

main :: IO()
main = do 
    print (distancia ((1, 1), (4, 5))) -- 5.0
    print (distancia3D ((1, 1, 1), (4, 5, 9))) -- 10.0