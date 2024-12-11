:- use_module(library(apply)).
:- use_module(library(lists)).

puzzle(
    0,
    [
        [0, 2, 1],
        [2, 1, 0],
        [1, 0, 2]
    ],
    [
        [1, 1, 2],
        [3, 4, 4],
        [3, 3, 4]
    ]
).

puzzle(
    1,
    [
        [2, 0, 0, 0, 1, 0],
        [0, 0, 3, 0, 0, 0],
        [0, 3, 0, 0, 5, 3],
        [0, 0, 0, 0, 0, 0],
        [0, 0, 3, 0, 4, 2],
        [0, 0, 0, 0, 0, 0]
    ],
    [
        [1, 1, 7, 7, 7, 11],
        [2, 2, 2, 2, 2, 11],
        [3, 6, 6, 6, 2, 10],
        [3, 3, 3, 6, 10, 10],
        [4, 4, 8, 9, 9, 9],
        [5, 5, 8, 8, 9, 9]
    ]
).

puzzle(
    8,
    [
        [0, 0, 0, 0, 0, 0, 0, 0],
        [0, 1, 3, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 3, 0, 0],
        [0, 0, 3, 0, 0, 0, 0, 0],
        [0, 5, 0, 3, 0, 0, 0, 0],
        [0, 2, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 3, 0],
        [0, 0, 5, 3, 0, 0, 0, 0]
    ],
    [
        [1, 1, 9, 9, 11, 13, 16, 16],
        [1, 1, 2, 9, 12, 13, 13, 16],
        [2, 2, 2, 7, 12, 14, 17, 17],
        [3, 3, 3, 7, 12, 14, 14, 17],
        [4, 7, 7, 7, 7, 14, 14, 17],
        [4, 5, 10, 10, 10, 15, 18, 17],
        [5, 5, 5, 5, 8, 15, 15, 15],
        [6, 8, 8, 8, 8, 15, 19, 19]
    ]
).


board(Id, Board) :- puzzle(Id, Board, _).
blocks(Id, Blocks) :- puzzle(Id, _, Blocks).

print_line([]).
print_line([H|T]) :- write(H), write(" "), print_line(T).

print_board([]).
print_board([H|T]) :- print_line(H), nl, print_board(T).

get_cell(Board, Row, Column, CellValue) :- nth0(Row, Board, Lista), nth0(Column, Lista, CellValue).

update_cell(Posicao, ValorAntigo, ValorNovo, Lista1, Lista2) :- 
    length(Pos, Posicao),
    append(Pos, [ValorAntigo|Resto], Lista1),
    append(Pos, [ValorNovo|Resto], Lista2).

update_board(Board, I, J, ValorNovo, NewBoard) :-
    update_cell(I, Antigo, Novo, Board, NewBoard),
    update_cell(J, _Antigo, ValorNovo, Antigo, Novo).

block_number(BoardID, Board, IdRegiao, CellValue) :-
    blocks(BoardID, MatrizRegioes),
    get_cell(MatrizRegioes, I, J, IdRegiao),
    get_cell(Board, I, J, CellValue).

block_numbers(BoardID, Board, IdRegiao, ListaNumerosRegiao) :- 
    findall(CellValue, block_number(BoardID, Board, IdRegiao, CellValue), ListaNumerosRegiao).

missing_numbers(Lista, ListaComplemento) :-
    length(Lista, Size),
    numlist(1, Size, ListaTotal),
    delete(Lista, 0, ListaResto),
    subtract(ListaTotal, ListaResto, ListaComplemento).

empty_cell(BoardID, Board, IdRegiao, [I, J]) :-
    blocks(BoardID, MatrizRegioes),
    get_cell(MatrizRegioes, I, J, IdRegiao),
    get_cell(Board, I, J, 0).

empty_cells(BoardID, Board, IdRegiao, ListaCoordenadasLivres) :- 
    findall([I, J], empty_cell(BoardID, Board, IdRegiao, [I, J]), ListaCoordenadasLivres).

verificarCoordenadas(BoardID, Board, I, J) :-
    blocks(BoardID, MatrizRegioes),
    get_cell(MatrizRegioes, I, J, IdRegiao),
    empty_cells(BoardID, Board, IdRegiao, ListaCoordenadasLivres),
    member([I, J], ListaCoordenadasLivres).

check_cell(BoardID, Board, I, J, CellValue) :-
    blocks(BoardID, MatrizRegioes),
    get_cell(MatrizRegioes, I, J, IdRegiao),
    block_numbers(BoardID, Board, IdRegiao, ListaNumerosRegiao),
    missing_numbers(ListaNumerosRegiao, ListaValoresFaltantes),
    member(CellValue, ListaValoresFaltantes).

verificarValor(Valor1, Valor2) :-
    Valor2 =:= 0 -> true; Valor1 < Valor2.

verificarMaiorRegiao(BoardID, Board, I, J, CellValue, Upper) :-
    blocks(BoardID, MatrizRegioes),
    get_cell(MatrizRegioes, I, J, IdRegiao),
    get_cell(MatrizRegioes, Upper, J, IdRegiaoCima),
    IdRegiao =:= IdRegiaoCima ->
    get_cell(Board, Upper, J, ValorCima),
    verificarValor(CellValue, ValorCima);
    get_cell(Board, Upper, J, ValorCima),
    CellValue =\= ValorCima.

verificarMenorRegiao(BoardID, Board, I, J, CellValue, Lower) :-
    blocks(BoardID, MatrizRegioes),
    get_cell(MatrizRegioes, I, J, IdRegiao),
    get_cell(MatrizRegioes, Lower, J, IdRegiaoBaixo),
    IdRegiao =:= IdRegiaoBaixo ->
    get_cell(Board, Lower, J, LowerCellValue), CellValue > LowerCellValue; 
    get_cell(Board, Lower, J, LowerCellValue), CellValue =\= LowerCellValue.

verify_upper(BoardID, Board, I, J, CellValue) :-
    Upper is (I - 1), Upper >= 0 ->
    verificarMaiorRegiao(BoardID, Board, I, J, CellValue, Upper); true.

verify_lower(BoardID, Board, I, J, CellValue) :-
    length(Board, Size),
    Lower is (I + 1), Lower < Size ->
    verificarMenorRegiao(BoardID, Board, I, J, CellValue, Lower); true.

verify_left(Board, I, J, CellValue) :-
    Left is (J - 1), Left >= 0 ->
    get_cell(Board, I, Left, LeftCellValue),
    CellValue =\= LeftCellValue; true.

verify_right(Board, I, J, CellValue) :- 
    length(Board, Size),
    Right is (J + 1), Right < Size ->
    get_cell(Board, I, Right, RightCellValue),
    CellValue =\= RightCellValue; true.

fill_position(BoardID, Board, I, J, NewBoard) :-
    verificarCoordenadas(BoardID, Board, I, J) ->
    check_cell(BoardID, Board, I, J, CellValue),
    verify_upper(BoardID, Board, I, J, CellValue),
    verify_lower(BoardID, Board, I, J, CellValue),
    verify_left(Board, I, J, CellValue),
    verify_right(Board, I, J, CellValue),
    update_board(Board, I, J, CellValue, NewBoard);
    NewBoard = Board.

fill_board(BoardID, Board, Row, Column, NewBoard) :-
    length(Board, Size),
    ( Row < Size, Column < Size ->
        fill_position(BoardID, Board, Row, Column, UpdatedBoard),
        NextColumn is Column + 1,
        ( NextColumn < Size ->
            fill_board(BoardID, UpdatedBoard, Row, NextColumn, NewBoard)
        ; NextRow is Row + 1,
          fill_board(BoardID, UpdatedBoard, NextRow, 0, NewBoard)
        )
    ; NewBoard = Board
    ).

solve(BoardID, MatrizInicial, MatrizFinal) :-
    fill_board(BoardID, MatrizInicial, 0, 0, MatrizFinal).

kojun(BoardID) :-
    board(BoardID, MatrizInicial), 
    solve(BoardID, MatrizInicial, MatrizFinal),
    nl, print_board(MatrizFinal), nl.
