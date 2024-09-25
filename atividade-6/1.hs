-- 1. Crie um tipo de dados Aluno, usando type, assim como criamos um tipo de dados Pessoa. O tipo Aluno
-- deve possuir um campo para o nome, outro para a disciplina e outros trˆes campos para notas. Agora,
-- execute os passos abaixo:
-- A: Crie uma fun¸c˜ao no mesmo estilo que a fun¸c˜ao pessoa, vista em sala e dispon´ıvel nos slides no Moodle,
-- ou seja, que receba um inteiro e retorne um Aluno correspondente ao valor inteiro.
-- B: Crie alguns alunos de exemplo, assim como tamb´em feito no exemplo da pessoa.
-- C: No main, imprima o primeiro nome de um aluno, portanto crie uma fun¸c˜ao para obter o primeiro
-- nome.
-- D: Crie uma fun¸c˜ao que receba um Int e retorne a m´edia do aluno correspondente.
-- E: Crie uma fun¸c˜ao que calcule a m´edia da turma, ou seja, considerando todos os alunos. DICA: crie
-- uma fun¸c˜ao recursiva que receba o primeiro identificador de aluno e incremente o identificador a cada
-- chamada recursiva, at´e chegar no ´ultimo aluno. N˜ao use listas!

type Aluno = (String, String, Float, Float, Float)

aluno :: Int -> Aluno
aluno 1 = ("Ana", "Matemática", 5.0, 6.0, 7.0)
aluno 2 = ("Bob", "Português", 6.0, 7.0, 8.0)
aluno 3 = ("Tom", "Física", 7.0, 8.0, 9.0)
aluno 4 = ("Lia", "Química", 8.0, 9.0, 10.0)
aluno 5 = ("Mia", "Biologia", 9.0, 10.0, 10.0)

getNome :: Aluno -> String
getNome (a,_,_,_,_) = a

getMedia :: Aluno -> Float
getMedia (_,_,a,b,c) = (a + b + c) / 3

getSomaMedias :: Aluno -> Float
getSomaMedias n = getMedia n + getSomaMedias (aluno n)

getMediaTurma :: Int -> Float
getMediaTurma 0 = 0
getMediaTurma n = getSomaMedias (aluno n) / fromIntegral n

main :: IO()
main = do
    print (getNome (aluno 1))
    print (getMedia (aluno 1))
    print (getMediaTurma 5)