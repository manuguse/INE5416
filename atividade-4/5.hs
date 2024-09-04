-- 5. Crie uma fun¸c˜ao que receba trˆes notas de um aluno (a, b, c), calcule a m´edia e retorne se o aluno foi
-- aprovado ou reprovado. Para um aluno ser aprovado, ele deve possuir nota igual ou superior a 6. Leia as
-- notas dos alunos do teclado

media :: Float -> Float -> Float -> String
media a b c = if (a + b + c) / 3 >= 6 then "Aprovado" else "Reprovado"

main :: IO ()
main = do
    putStrLn "Digite a primeira nota: "
    a <- getLine
    putStrLn "Digite a segunda nota: "
    b <- getLine
    putStrLn "Digite a terceira nota: "
    c <- getLine
    print (media (read a) (read b) (read c))