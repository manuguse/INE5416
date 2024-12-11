:- use_module(library(apply)).
:- use_module(library(lists)).

% ------------- define puzzles -------------
% define todos os puzzles com um ID, a matriz a ser resolvida e os blocos 

puzzle(1, [
    [2, 0, 0, 0, 1, 0],
    [0, 0, 0, 3, 0, 0],
    [0, 3, 0, 0, 5, 3],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 3, 0, 4, 2],
    [0, 0, 0, 0, 0, 0]
], [
    [1, 1, 7, 7, 7, 11],
    [2, 2, 2, 2, 2, 11],
    [3, 6, 6, 6, 2, 10],
    [3, 3, 3, 6, 10, 10],
    [4, 4, 8, 9, 9, 9],
    [5, 5, 8, 8, 9, 9]
]).

puzzle(0, [
    [0, 2, 1],
    [2, 1, 0],
    [1, 0, 2]
], [
    [1, 1, 2],
    [3, 4, 4],
    [3, 3, 4]
]).

puzzle(8, [
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 1, 3, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 3, 0, 0],
    [0, 0, 3, 0, 0, 0, 0, 0],
    [0, 5, 0, 3, 0, 0, 0, 0],
    [0, 2, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 3, 0],
    [0, 0, 5, 3, 0, 0, 0, 0]
], [
    [1, 1, 9, 9, 11, 13, 16, 16],
    [1, 1, 2, 9, 12, 13, 13, 16],
    [2, 2, 2, 7, 12, 14, 17, 17],
    [3, 3, 3, 7, 12, 14, 14, 17],
    [4, 7, 7, 7, 7, 14, 14, 17],
    [4, 5, 10, 10, 10, 15, 18, 17],
    [5, 5, 5, 5, 8, 15, 15, 15],
    [6, 8, 8, 8, 8, 15, 19, 19]
]).

% ------------- funções auxiliares -------------

% board(Id, Board) retorna a matriz do puzzle com o IDs
board(Id, Board) :- puzzle(Id, Board, _).

% blocks(Id, Blocks) retorna a matriz de blocos do puzzle com o ID
blocks(Id, Blocks) :- puzzle(Id, _, Blocks).

% printa uma linha da matriz
print_line([]).
print_line([H|T]) :- write(H), write(" "), print_line(T).

% printa a matriz inteira (tabuleiro)
print_board([]).
print_board([H|T]) :- print_line(H), nl, print_board(T).

% retorna o valor da célula na linha Row e coluna Column
get_cell(Board, Row, Column, CellValue) :- nth0(Row, Board, Line), nth0(Column, Line, CellValue).

% atualiza o valor da célula na linha Row e coluna Column
update_cell(Posicao, OldValue, NewValue, LineP1, LineP2) :- 
    length(Pos, Posicao), % pega o tamanho até a posição desejada
    append(Pos, [OldValue|Resto], LineP1), % pega a linha até a posição desejada
    append(Pos, [NewValue|Resto], LineP2). % concatena a linha até a posição desejada com o novo valor

% atualiza o valor da célula na linha I e coluna J
update_board(Board, I, J, NewValue, NewBoard) :-
    update_cell(I, Old, New, Board, NewBoard), % atualiza a linha I
    update_cell(J, _, NewValue, Old, New).  % atualiza a coluna J

% retorna a lista de valores de um bloco
block_number(BoardID, Board, BlockID, CellValue) :-
    blocks(BoardID, Blocks), % pega a matriz do bloco desejado
    get_cell(Blocks, I, J, BlockID), % pega a posição do bloco
    get_cell(Board, I, J, CellValue). % pega o valor da célula

% retorna a lista de valores de um bloco
block_numbers(BoardID, Board, BlockID, BlockNumbers) :- 
    % vai pegar todos os valores com o ID do bloco desejado
    findall(CellValue, block_number(BoardID, Board, BlockID, CellValue), BlockNumbers).

% retorna a lista de valores faltantes em um bloco
missing_numbers(List, MissingNumbers) :-
    length(List, Size), % pega o tamanho da linha
    numlist(1, Size, AllNumbers), % cria uma lista de 1 até o tamanho do bloco
    delete(List, 0, FilledNumbers), % deleta os valores 0 da lista, pois são os vazios
    subtract(AllNumbers, FilledNumbers, MissingNumbers). % subtrai os valores preenchidos da lista de todos os valores

% retorna a lista de células vazias de um bloco
empty_cell(BoardID, Board, BlockID, [I, J]) :-
    blocks(BoardID, Blocks), % pega a matriz de blocos
    get_cell(Blocks, I, J, BlockID), % pega a posição do bloco
    get_cell(Board, I, J, 0). % verifica se a célula está vazia

% retorna a lista de células vazias de um bloco
empty_cells(BoardID, Board, BlockID, EmptyCells) :-
    % acha toda as células que estão vazias 
    findall([I, J], empty_cell(BoardID, Board, BlockID, [I, J]), EmptyCells).

% pega as células vazias de um bloco
is_missing_cell(BoardID, Board, I, J) :-
    blocks(BoardID, Blocks), % pega a matriz de blocos
    get_cell(Blocks, I, J, BlockID), % pega a posição do bloco
    empty_cells(BoardID, Board, BlockID, EmptyCells), % lista de células vazias
    member([I, J], EmptyCells). % verifica se a célula está vazia (faz parte da lista de células vazias)

% verifica se o valor da célula é válido
check_cell(BoardID, Board, I, J, CellValue) :-
    blocks(BoardID, Blocks), % obtem a matriz de blocos
    get_cell(Blocks, I, J, BlockID), % pega o valor da célula na posição (I, J)
    block_numbers(BoardID, Board, BlockID, BlockNumbers), % pega a lista de valores do bloco
    missing_numbers(BlockNumbers, MissingValues), % verifica quais valores estão faltando
    member(CellValue, MissingValues). % verifica se o valor da célula está na lista de valores faltantes, ou seja, ainda não está no bloco

% verifica se v1 é menor que v2
less_then(V1, V2) :-
    V2 =:= 0 -> true; V1 < V2.

% verifica se o valor da célula é maior e diferente que o valor da célula de cima
check_bigger_block(BoardID, Board, I, J, CellValue, Upper) :-
    blocks(BoardID, Blocks), % obtem o bloco com esse id
    get_cell(Blocks, I, J, BlockID), % pega o valor da célula na posição (I, J) 
    get_cell(Blocks, Upper, J, UpperBlockID), % pega o valor da célula na posição (Upper, J)
    BlockID =:= UpperBlockID -> % verifica se os IDs dos blocos são iguais
    get_cell(Board, Upper, J, UpperValue), % se for, pega o valor da célula na posição (Upper, J) no tabuleiro
    less_then(CellValue, UpperValue); % verifica se o valor da célula na posição (I, J) é menor que o valor da célula na posição (Upper, J)
    get_cell(Board, Upper, J, UpperValue), % se não for, pega o valor da célula na posição (Upper, J) no tabuleiro
    CellValue =\= UpperValue. % verifica se o valor da célula na posição (I, J) é diferente do valor da célula na posição (Upper, J)

% verifica se o valor da célula é menor e diferente que o valor da célula de baixo
check_smaller_block(BoardID, Board, I, J, CellValue, Lower) :-
    blocks(BoardID, Blocks), % obtem o bloco com esse id
    get_cell(Blocks, I, J, BlockID), % pega o valor da célula na posição (I, J)
    get_cell(Blocks, Lower, J, LowerBlockID), % pega o valor da célula na posição (Lower, J)
    BlockID =:= LowerBlockID -> % verifica se os IDs dos blocos são iguais
    get_cell(Board, Lower, J, LowerCellValue), % se for, pega o valor da célula na posição (Lower, J) no tabuleiro
    CellValue > LowerCellValue; % verifica se o valor da célula na posição (I, J) é maior que o valor da célula na posição (Lower, J)
    get_cell(Board, Lower, J, LowerCellValue), % se não for, pega o valor da célula na posição (Lower, J) no tabuleiro
    CellValue =\= LowerCellValue. % verifica se o valor da célula na posição (I, J) é diferente do valor da célula na posição (Lower, J)

% verifica se o valor da célula é maior e diferente que o valor da célula de cima
verify_upper(BoardID, Board, I, J, CellValue) :-
    Upper is (I - 1), Upper >= 0 ->
    check_bigger_block(BoardID, Board, I, J, CellValue, Upper); true.

% verifica se o valor da célula é menor e diferente que o valor da célula de baixo
verify_lower(BoardID, Board, I, J, CellValue) :-
    length(Board, Size),
    Lower is (I + 1), Lower < Size ->
    check_smaller_block(BoardID, Board, I, J, CellValue, Lower); true.

% acima vai ser válida se o valor da célula for menor que o valor da célula de cima
verify_left(Board, I, J, CellValue) :-
    Left is (J - 1), Left >= 0 ->
    get_cell(Board, I, Left, LeftCellValue),
    CellValue =\= LeftCellValue; true.

% a direita vai ser valida se o valor da célula for diferente da célula da direita
verify_right(Board, I, J, CellValue) :- 
    length(Board, Size),
    Right is (J + 1), Right < Size ->
    get_cell(Board, I, Right, RightCellValue),
    CellValue =\= RightCellValue; true.

% -------------- funções principais --------------

% verifica se o valor da célula é válido, chamando as funções auxiliares
verify(BoardID, Board, I, J, CellValue) :-
    verify_upper(BoardID, Board, I, J, CellValue),
    verify_lower(BoardID, Board, I, J, CellValue),
    verify_left(Board, I, J, CellValue),
    verify_right(Board, I, J, CellValue).

% tenta preencher a posição I, J e ver se é possível
fill_position(BoardID, Board, I, J, NewBoard) :-
    is_missing_cell(BoardID, Board, I, J) -> % verifica se a célula está vazia
    check_cell(BoardID, Board, I, J, CellValue), % verifica se o valor é válido
    verify(BoardID, Board, I, J, CellValue), % verifica se o valor é válido
    update_board(Board, I, J, CellValue, NewBoard); % atualiza o tabuleiro com o valor da célula
    NewBoard = Board. % se não for possível, retorna o tabuleiro sem alterações

% preenche o tabuleiro recursivamente
fill_board(BoardID, Board, Row, Column, NewBoard) :-
    length(Board, Size), % pega o tamanho do tabuleiro
    ( Row < Size, Column < Size -> % verifica se a linha e a coluna são menores que o tamanho do tabuleiro, sendo um valor válido
        % se for, tenta preencher a posição e chama a função recursivamente
        fill_position(BoardID, Board, Row, Column, UpdatedBoard), 
        NextColumn is Column + 1, % incrementa a coluna
        ( NextColumn < Size -> % verifica se a próxima coluna é menor que o tamanho do tabuleiro
            fill_board(BoardID, UpdatedBoard, Row, NextColumn, NewBoard) % chama a função recursivamente
        ; NextRow is Row + 1, % incrementa a linha
          fill_board(BoardID, UpdatedBoard, NextRow, 0, NewBoard) % chama a função recursivamente
        )
    ; NewBoard = Board % se a linha e a coluna forem maiores que o tamanho do tabuleiro, retorna o tabuleiro sem alterações
    ).

% tenta resolver o tabuleiro, iniciando pela posição 0, 0
solve(BoardID, MatrizInicial, MatrizFinal) :- 
    fill_board(BoardID, MatrizInicial, 0, 0, MatrizFinal).

% função principal para resolver o tabuleiro
kojun(BoardID) :-
    board(BoardID, MatrizInicial), % pega a matriz inicial
    solve(BoardID, MatrizInicial, MatrizFinal), % resolve o tabuleiro
    nl, print_board(MatrizFinal), nl. % printa o tabuleiro final
