% Definição dos puzzles
puzzle(1, [
    [2, _, _, _, 1, _],
    [_, _, _, 3, _, _],
    [_, 3, _, _, 5, 3],
    [_, _, _, _, _, _],
    [_, _, 3, _, 4, 2],
    [_, _, _, _, _, _]
], [
    [1, 1, 7, 7, 7, 11],
    [2, 2, 2, 2, 2, 11],
    [3, 6, 6, 6, 2, 10],
    [3, 3, 3, 6, 10, 10],
    [4, 4, 8, 9, 9, 9],
    [5, 5, 8, 8, 9, 9]
]).

all_distinct([]).
all_distinct([H|T]) :- \+ member(H, T), all_distinct(T).

% Verifica se uma célula está vazia.
is_empty(X) :- var(X).

% Encontra uma célula vazia no tabuleiro.
find_empty_cell(Board, Row, Col) :-
    nth0(Row, Board, Line),
    nth0(Col, Line, Cell),
    is_empty(Cell).

% Atualiza uma célula no tabuleiro.
update_board(Board, Row, Col, Value, UpdatedBoard) :-
    nth0(Row, Board, Line, RestRows),
    nth0(Col, Line, _, RestCols),
    nth0(Col, NewLine, Value, RestCols),
    nth0(Row, UpdatedBoard, NewLine, RestRows).

% Verifica se não há números repetidos no bloco.
no_repeated_numbers_in_block(Board, Blocks) :-
    flatten(Board, FlatBoard),
    flatten(Blocks, FlatBlocks),
    findall((BlockId, Num), (nth0(Index, FlatBlocks, BlockId), nth0(Index, FlatBoard, Num), nonvar(Num)), Pairs),
    group_pairs_by_key(Pairs, Groups),
    maplist(no_duplicates, Groups).

no_duplicates((_, Numbers)) :-
    sort(Numbers, Sorted),
    length(Numbers, Len),
    length(Sorted, Len).

% Verifica se não há números repetidos em células adjacentes.
no_repeated_numbers_in_adjacent_cells(Board) :-
    \+ (adjacent(Board, Row1, Col1, Row2, Col2),
        nth0(Row1, Board, Line1), nth0(Col1, Line1, Value),
        nth0(Row2, Board, Line2), nth0(Col2, Line2, Value),
        nonvar(Value)).

adjacent(Board, Row1, Col1, Row2, Col2) :-
    length(Board, N),
    between(0, N, Row1), between(0, N, Col1),
    neighbor(Row1, Col1, Row2, Col2).

neighbor(Row, Col, Row, Col2) :- Col2 is Col + 1.
neighbor(Row, Col, Row2, Col) :- Row2 is Row + 1.

% Verifica se todas as restrições são satisfeitas.
verify(Board, Blocks) :-
    no_repeated_numbers_in_block(Board, Blocks),
    no_repeated_numbers_in_adjacent_cells(Board).

% Encontra valores possíveis para uma célula.
valid_values(Board, Blocks, Row, Col, Value) :-
    member(Value, [1, 2, 3, 4, 5, 6, 7, 8, 9]), % Valores possíveis
    update_board(Board, Row, Col, Value, UpdatedBoard),
    verify(UpdatedBoard, Blocks).

% Função principal que resolve o tabuleiro.
solve(Board, Blocks) :-
    find_empty_cell(Board, Row, Col), % Localiza uma célula vazia
    valid_values(Board, Blocks, Row, Col, Value), % Encontra um valor válido
    update_board(Board, Row, Col, Value, UpdatedBoard), % Atualiza o tabuleiro
    solve(UpdatedBoard, Blocks). % Resolve o tabuleiro recursivamente
solve(Board, _) :- \+ find_empty_cell(Board, _, _). % Termina se não houver células vazias.

% Função para resolver um puzzle Kojun com base no ID
solucao(ID, TabuleiroResposta) :-
    integer(ID), % Verifica se ID é um inteiro
    puzzle(ID, TabuleiroProblema, Blocks),
    solve(TabuleiroProblema, Blocks),
    extract_second_values(TabuleiroProblema, TabuleiroResposta).

% Extrai o segundo valor de cada matriz
extract_second([], []).
extract_second([[_, Second] | Rest], [Second | SecondValues]) :-
    extract_second(Rest, SecondValues).

% Extrai os valores de cada linha da matriz
extract_second_values([], []).
extract_second_values([Sublist | Rest], [SecondValues | Result]) :-
    extract_second(Sublist, SecondValues),
    extract_second_values(Rest, Result).