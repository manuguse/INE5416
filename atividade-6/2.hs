--  2 Altere nosso exemplo da forma e inclua uma nova forma (Triangulo) no construtor do tipo Forma e tamb´em
-- calcule sua ´area.

data Forma = Circulo Float | Retangulo Float Float | Triangulo Float Float

area :: Forma -> Float
area (Circulo r) = pi * r^2
area (Retangulo b h) = b * h
area (Triangulo b h) = (b * h) / 2

main :: IO()
main = do
    print (area (Circulo 3)) -- 28.274334
    print (area (Retangulo 3 4)) -- 12.0
    print (area (Triangulo 3 4)) -- 6.0