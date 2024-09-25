-- 7. Motifique o arquivo alunos.hs (dispon´ıvel no Moodle) de forma a adicionar novas fun¸c˜oes:

-- A: Crie uma fun¸c˜ao com a seguinte assinatura: aprovados :: [(Int, String, Float)] -> [String],
-- a qual recebe uma lista de alunos e retorna uma lista com o nome dos alunos aprovados. Um aluno
-- est´a aprovado se a sua m´edia ´e maior ou igual a 6. Utilize map e filter para resolver esta quest˜ao.

-- B: Crie uma fun¸c˜ao com a seguinte assinatura: aprovados2 :: [(Int, String, Float)] -> [String],
-- a qual recebe uma lista de alunos e retorna uma lista com o nome dos alunos aprovados. Um aluno
-- est´a aprovado se a sua m´edia ´e maior ou igual a 6. N˜ao utilize map e filter para resolver esta
-- quest˜ao. Utilize o conceito de list comprehension.

-- C: Utilize (e modifique, se necess´ario) a fun¸c˜ao gerarPares vista em aula e dispon´ıvel no arquivo
-- alunos.hs para formar duplas de alunos. Note que um aluno n˜ao pode fazer dupla consigo mesmo.

alunos :: [(Int, String, Float)]
alunos = [(1, "Ana", 3.4), (2, "Bob", 6.7), (3, "Tom", 7.6), (4, "Lia", 9.0), (5, "Mia", 5.5)]

getNome :: (Int, String, Float) -> String
getNome (a,b,c) = b

getPrimeiroAluno :: [(Int, String, Float)] -> (Int, String, Float)
getPrimeiroAluno (a:_) = a

gerarPares :: Eq t => [t] -> [t] -> [(t,t)] 
gerarPares l1 l2 = [(a,b) | a <- l1, b <- l2, a /= b]

aprovados :: [(Int, String, Float)] -> [String]
aprovados alunos = map getNome (filter (\(_,_,c) -> c >= 6) alunos) 
                                    -- idx, nome, media, se media >= 6, retorna nome

aprovados2 :: [(Int, String, Float)] -> [String]
aprovados2 alunos = [getNome aluno | aluno <- alunos, getMedia aluno >= 6]
    where getMedia (_,_,media) = media

main = do
    print (aprovados alunos)
    print (aprovados2 alunos)
    print (gerarPares alunos alunos)