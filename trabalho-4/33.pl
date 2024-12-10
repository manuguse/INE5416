% problema: https://rachacuca.com.br/logica/problemas/campeonato-de-surf/

% ------------ definindo as variáveis ------------

cor(amarela).
cor(azul).
cor(branca).
cor(verde).
cor(vermelha).

nome(flavio).
nome(fernando).
nome(igor).
nome(rogerio).
nome(tomas).

praia(areado).
praia(brava).
praia(itaguare).
praia(itamambuca).
praia(maresias).

tamanho(56).
tamanho(57).
tamanho(58).
tamanho(511).
tamanho(60).

gentilico(gaucho).
gentilico(baiano).
gentilico(fluminense).
gentilico(mineiro).
gentilico(paulista).

ranking(1).
ranking(2).
ranking(3).
ranking(5).
ranking(8).

% ------------ funções auxiliares ------------

% verifica se X está à esquerda de Y
aEsquerda(X, Y, Lista) :- nth0(IndexX, Lista, X), nth0(IndexY, Lista, Y), IndexX < IndexY.

% verifica se X está exatamente à esquerda de Y
exatamenteAEsquerda(X, Y, Lista) :- nextto(X, Y, Lista).

% vai ser a mesma coisa que aEsquerda, mas invertendo X e Y
aDireita(X, Y, Lista) :- aEsquerda(Y, X, Lista).

% verifica se X está exatamente à direita de Y, invertendo X e Y
exatamenteADireita(X, Y, Lista) :- exatamenteAEsquerda(Y, X, Lista).

% verifica se X e Y estão um ao lado do outro na lista
aoLado(X, Y, Lista) :- nextto(X, Y, Lista); nextto(Y, X, Lista).

% a lista terá todos os elementos diferentes se não houver repetição de elementos
todosDiferentes([]).
todosDiferentes([H|T]) :- not(member(H, T)), todosDiferentes(T).

% x vai estar entre y e z se y estiver à esquerda de x e z estiver à direita de x
entre(X, Y, Z, Lista) :- aEsquerda(X, Z, Lista), aDireita(X, Y, Lista).

% ------------ regras ------------

% cria a lista de solucao e chama a funcao que resolve o problemas
solucao(ListaSolucao) :-
    ListaSolucao = [surfista(Cor1, Nome1, Praia1, Tamanho1, Gentilico1, Ranking1),
                    surfista(Cor2, Nome2, Praia2, Tamanho2, Gentilico2, Ranking2),
                    surfista(Cor3, Nome3, Praia3, Tamanho3, Gentilico3, Ranking3),
                    surfista(Cor4, Nome4, Praia4, Tamanho4, Gentilico4, Ranking4),
                    surfista(Cor5, Nome5, Praia5, Tamanho5, Gentilico5, Ranking5)],


% O surfista que está no Quinto lugar do ranking está na quarta posição.
nth0(3, ListaSolucao, surfista(_, _, _, _, _, 5)),

% O Oitavo do ranking está exatamente à esquerda do Quinto lugar.
exatamenteAEsquerda(surfista(_, _, _, _, _, 8), surfista(_, _, _, _, _, 5), ListaSolucao),

% O Primeiro do ranking está em algum lugar à direita do surfista da prancha Amarela.
aDireita(surfista(_, _, _, _, _, 1), surfista(amarela, _, _, _, _, _), ListaSolucao),

% O Mineiro está exatamente à direita do surfista que está em Segundo no ranking.
exatamenteADireita(surfista(_, _, _, _, mineiro, _), surfista(_, _, _, _, _, 2), ListaSolucao),

% O dono da prancha Azul está em algum lugar à esquerda do Paulista.
aEsquerda(surfista(azul, _, _, _, _, _), surfista(_, _, _, _, paulista, _), ListaSolucao),

% O Gaúcho está em algum lugar à direita do surfista da prancha Verde.
aDireita(surfista(_, _, _, _, gaucho, _), surfista(verde, _, _, _, _, _), ListaSolucao),

% O Mineiro está ao lado do Oitavo do ranking.
aoLado(surfista(_, _, _, _, mineiro, _), surfista(_, _, _, _, _, 8), ListaSolucao),

% O Baiano está exatamente à direita do Mineiro.
exatamenteADireita(surfista(_, _, _, _, baiano, _), surfista(_, _, _, _, mineiro, _), ListaSolucao),

% O surfista da prancha de 6'0" está na quinta posição.
nth0(4, ListaSolucao, surfista(_, _, _, 60, _, _)),

% O dono da prancha de 5'8" está em algum lugar entre o Tomas e o dono da prancha de 5'7".
entre(surfista(_, _, _, 58, _, _), surfista(_, tomas, _, _, _, _), surfista(_, _, _, 57, _, _),  ListaSolucao),

% O surfista da prancha de 5'11" está em algum lugar entre o Segundo do ranking e o surfista da prancha de 5'8".
entre(surfista(_, _, _, 511, _, _), surfista(_, _, _, _, _, 2), surfista(_, _, _, 58, _, _), ListaSolucao),

% O surfista que curte surfar em Maresias está exatamente à direita do surfista da prancha de 5'7".
exatamenteADireita(surfista(_, _, maresias, _, _, _), surfista(_, _, _, 57, _, _), ListaSolucao),

% O dono da prancha Amarela está ao lado do surfista que gosta de surfar em Areado.
aoLado(surfista(amarela, _, _, _, _, _), surfista(_, _, areado, _, _, _), ListaSolucao),

% Quem gosta de surfar em Itamambuca está exatamente à esquerda de quem gosta de surfar em Areado.
exatamenteAEsquerda(surfista(_, _, itamambuca, _, _, _), surfista(_, _, areado, _, _, _), ListaSolucao),

% O surfista da prancha Amarela está em algum lugar entre o que curte surfar em Brava e o Quinto do ranking.
entre(surfista(amarela, _, _, _, _, _), surfista(_, _, brava, _, _, _), surfista(_, _, _, _, _, 5), ListaSolucao),

% Rogério está ao lado do surfista que nasceu na Bahia.
aoLado(surfista(_, rogerio, _, _, _, _), surfista(_, _, _, _, baiano, _), ListaSolucao),

% Igor está em algum lugar à direita do dono da prancha Verde.
aDireita(surfista(_, igor, _, _, _, _), surfista(verde, _, _, _, _, _), ListaSolucao),

% O surfista da prancha Amarela está em algum lugar à esquerda do Flávio.
aEsquerda(surfista(amarela, _, _, _, _, _), surfista(_, flavio, _, _, _, _), ListaSolucao),

% Fernando está em algum lugar entre o dono da prancha Vermelha e o Rogério.
entre(surfista(_, fernando, _, _, _, _), surfista(vermelha, _, _, _, _, _), surfista(_, rogerio, _, _, _, _), ListaSolucao),

% O Gaúcho está na Quarta posição.
nth0(3, ListaSolucao, surfista(_, _, _, _, gaucho, _)),

% O dono da prancha Amarela está em algum lugar entre o surfista da prancha Vermelha e o que curte surfar em Itaguaré.
entre(surfista(amarela, _, _, _, _, _), surfista(vermelha, _, _, _, _, _), surfista(_, _, itaguare, _, _, _), ListaSolucao),

% ------------ chamando a funcao solucao ------------

cor(Cor1),
cor(Cor2),
cor(Cor3),
cor(Cor4),
cor(Cor5),
todosDiferentes([Cor1, Cor2, Cor3, Cor4, Cor5]),

nome(Nome1),
nome(Nome2),
nome(Nome3),
nome(Nome4),
nome(Nome5),
todosDiferentes([Nome1, Nome2, Nome3, Nome4, Nome5]),

praia(Praia1),
praia(Praia2),
praia(Praia3),
praia(Praia4),
praia(Praia5),
todosDiferentes([Praia1, Praia2, Praia3, Praia4, Praia5]),

tamanho(Tamanho1),
tamanho(Tamanho2),
tamanho(Tamanho3),
tamanho(Tamanho4),
tamanho(Tamanho5),
todosDiferentes([Tamanho1, Tamanho2, Tamanho3, Tamanho4, Tamanho5]),

gentilico(Gentilico1),
gentilico(Gentilico2),
gentilico(Gentilico3),
gentilico(Gentilico4),
gentilico(Gentilico5),
todosDiferentes([Gentilico1, Gentilico2, Gentilico3, Gentilico4, Gentilico5]),

ranking(Ranking1),
ranking(Ranking2),
ranking(Ranking3),
ranking(Ranking4),
ranking(Ranking5),
todosDiferentes([Ranking1, Ranking2, Ranking3, Ranking4, Ranking5]),

% ------------ printando a solucao ------------
write(ListaSolucao), nl.