:- use_module(library(apply)).
:- use_module(library(lists)).

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

board(Id, Board) :- puzzle(Id, Board, _).
blocks(Id, Blocks) :- puzzle(Id, _, Blocks).

print_line([]).
print_line([H|T]) :- write(H), write(" "), print_line(T).

print_board([]).
print_board([H|T]) :- print_line(H), nl, print_board(T).

get_cell(Board, Row, Column, CellValue) :- nth0(Row, Board, Line), nth0(Column, Line, CellValue).

update_cell(Posicao, OldValue, NewValue, LineP1, LineP2) :- 
    length(Pos, Posicao),
    append(Pos, [OldValue|Resto], LineP1),
    append(Pos, [NewValue|Resto], LineP2).

update_board(Board, I, J, NewValue, NewBoard) :-
    update_cell(I, Old, New, Board, NewBoard),
    update_cell(J, _, NewValue, Old, New).

block_number(BoardID, Board, BlockID, CellValue) :-
    blocks(BoardID, Blocks),
    get_cell(Blocks, I, J, BlockID),
    get_cell(Board, I, J, CellValue).

block_numbers(BoardID, Board, BlockID, BlockNumbers) :- 
    findall(CellValue, block_number(BoardID, Board, BlockID, CellValue), BlockNumbers).

missing_numbers(Line, MissingNumbers) :-
    length(Line, Size),
    numlist(1, Size, AllNumbers),
    delete(Line, 0, FilledNumbers),
    subtract(AllNumbers, FilledNumbers, MissingNumbers).

empty_cell(BoardID, Board, BlockID, [I, J]) :-
    blocks(BoardID, Blocks),
    get_cell(Blocks, I, J, BlockID),
    get_cell(Board, I, J, 0).

empty_cells(BoardID, Board, BlockID, EmptyCells) :- 
    findall([I, J], empty_cell(BoardID, Board, BlockID, [I, J]), EmptyCells).

missing_cells(BoardID, Board, I, J) :-
    blocks(BoardID, Blocks),
    get_cell(Blocks, I, J, BlockID),
    empty_cells(BoardID, Board, BlockID, EmptyCells),
    member([I, J], EmptyCells).

check_cell(BoardID, Board, I, J, CellValue) :-
    blocks(BoardID, Blocks),
    get_cell(Blocks, I, J, BlockID),
    block_numbers(BoardID, Board, BlockID, BlockNumbers),
    missing_numbers(BlockNumbers, MissingValues),
    member(CellValue, MissingValues).

check_value(V1, V2) :-
    V2 =:= 0 -> true; V1 < V2.

check_bigger_block(BoardID, Board, I, J, CellValue, Upper) :-
    blocks(BoardID, Blocks),
    get_cell(Blocks, I, J, BlockID),
    get_cell(Blocks, Upper, J, UpperBlockID),
    BlockID =:= UpperBlockID ->
    get_cell(Board, Upper, J, UpperValue),
    check_value(CellValue, UpperValue);
    get_cell(Board, Upper, J, UpperValue),
    CellValue =\= UpperValue.

check_smaller_block(BoardID, Board, I, J, CellValue, Lower) :-
    blocks(BoardID, Blocks),
    get_cell(Blocks, I, J, BlockID),
    get_cell(Blocks, Lower, J, LowerBlockID),
    BlockID =:= LowerBlockID ->
    get_cell(Board, Lower, J, LowerCellValue), CellValue > LowerCellValue; 
    get_cell(Board, Lower, J, LowerCellValue), CellValue =\= LowerCellValue.

verify_upper(BoardID, Board, I, J, CellValue) :-
    Upper is (I - 1), Upper >= 0 ->
    check_bigger_block(BoardID, Board, I, J, CellValue, Upper); true.

verify_lower(BoardID, Board, I, J, CellValue) :-
    length(Board, Size),
    Lower is (I + 1), Lower < Size ->
    check_smaller_block(BoardID, Board, I, J, CellValue, Lower); true.

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
    missing_cells(BoardID, Board, I, J) ->
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
