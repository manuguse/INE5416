:- use_module(library(clpfd)).

% Definição dos puzzles
puzzle(
    1,
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
    2,
    [
        [2, 0, 0, 0, 1, 0],
        [0, 0, 3, 0, 0, 0],
        [0, 3, 0, 5, 3, 0],
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

board(ID, Board) :-
    puzzle(ID, Board, _).

blocks(ID, Blocks) :-
    puzzle(ID, _, Blocks).

printList([]).
printList([H|T]) :- write(H), write(" "), printList(T).

printMatrix([]).
printMatrix([H|T]) :- printList(H), nl, printMatrix(T).

get_position(Board, I, J, Found) :- 
    nth0(I, Board, Line),
    nth0(J, Line, Found).

set_position(Position, Value, CurrentList, NewList) :- 
    length(Prefix, Position),
    append(Prefix, [_|Suffix], CurrentList),
    append(Prefix, [Value|Suffix], NewList).

update_board(Board, I, J, Value, NewBoard) :-
    nth0(I, Board, OldRow, RestRows),
    set_position(J, Value, OldRow, NewRow),
    nth0(I, NewBoard, NewRow, RestRows).

searchNumberOnRegion(Board, Blocks, BlockId, Result) :-
    get_position(Blocks, I, J, BlockId),
    get_position(Board, I, J, Result).

block_numbers(Board, Blocks, BlockId, FinalList) :- 
    findall(FoundNumber, searchNumberOnRegion(Board, Blocks, BlockId, FoundNumber), FinalList).

complement(List, Complement) :-
    length(List, Length),
    numlist(1, Length, UniverseList),
    delete(List, 0, RestList),
    subtract(UniverseList, RestList, Complement).

is_empty_cell(Board, Blocks, BlockId, [I, J]) :-
    get_position(Blocks, I, J, BlockId),
    get_position(Board, I, J, 0).

get_empty_cells(Board, Blocks, BlockId, ZeroedPositions) :- 
    findall([I, J], is_empty_cell(Board, Blocks, BlockId, [I, J]), ZeroedPositions).

verify_empty(Board, Blocks, I, J) :-
    get_position(Blocks, I, J, BlockId),
    get_empty_cells(Board, Blocks, BlockId, ListOfZeroedPositions),
    member([I, J], ListOfZeroedPositions).

get_positionPossibleForPosition(Board, Blocks, I, J, PossibleValue) :-
    get_position(Blocks, I, J, BlockId),
    block_numbers(Board, Blocks, BlockId, RegionNumbers),
    complement(RegionNumbers, Complement),
    member(PossibleValue, Complement).

verifyAboveGreater(BelowValue, AboveValue) :-
    AboveValue =:= 0 -> true; BelowValue < AboveValue.

verifyAbove(Board, Blocks, I, J, Value, IAbove) :-
    get_position(Blocks, I, J, BlockId),
    get_position(Blocks, IAbove, J, AboveBlockId),
    BlockId =:= AboveBlockId ->
    get_position(Board, IAbove, J, AboveValue),
    verifyAboveGreater(Value, AboveValue);
    get_position(Board, IAbove, J, AboveValue),
    Value =\= AboveValue.

verifyBelow(Board, Blocks, I, J, Value, IBelow) :-
    get_position(Blocks, I, J, BlockId),
    get_position(Blocks, IBelow, J, IdRegiaoBaixo),
    BlockId =:= IdRegiaoBaixo ->
    get_position(Board, IBelow, J, BelowValue), Value > BelowValue; 
    get_position(Board, IBelow, J, BelowValue), Value =\= BelowValue.

verifyUp(Board, Blocks, I, J, PossibleValue) :-
    IAbove is (I - 1), IAbove >= 0 ->
    verifyAbove(Board, Blocks, I, J, PossibleValue, IAbove); true.

verifyDown(Board, Blocks, I, J, PossibleValue) :-
    length(Board, Length),
    IBelow is (I + 1), IBelow < Length ->
    verifyBelow(Board, Blocks, I, J, PossibleValue, IBelow); true.

verifyLeft(Board, I, J, Value) :-
    Left is (J - 1), Left >= 0 ->
    get_position(Board, I, Left, LeftValue),
    Value =\= LeftValue; true.

verifyRight(Board, I, J, Value) :- 
    length(Board, Length),
    IRight is (J + 1), IRight < Length ->
    get_position(Board, I, IRight, RightValue),
    Value =\= RightValue; true.

fillPosition(Board, Blocks, I, J, NewBoard) :-
    verify_empty(Board, Blocks, I, J) ->
    get_positionPossibleForPosition(Board, Blocks, I, J, PossibleValue),
    verifyUp(Board, Blocks, I, J, PossibleValue),
    verifyDown(Board, Blocks, I, J, PossibleValue),
    verifyLeft(Board, I, J, PossibleValue),
    verifyRight(Board, I, J, PossibleValue),
    update_board(Board, I, J, PossibleValue, NewBoard);
    NewBoard = Board.

solveKojun(OriginalBoard, Blocks, I, J, SolvedBoard) :-
    length(OriginalBoard, N),
    (I >= N -> SolvedBoard = OriginalBoard;
    (J >= N -> NI is I + 1, NJ is 0, solveKojun(OriginalBoard, Blocks, NI, NJ, SolvedBoard);
    (fillPosition(OriginalBoard, Blocks, I, J, UpdatedBoard),
    NJ is J + 1,
    solveKojun(UpdatedBoard, Blocks, I, NJ, SolvedBoard)))).

kojun(ID) :-
    board(ID, OriginalBoard),
    blocks(ID, Blocks),
    writeln('Board:'),
    printMatrix(OriginalBoard),
    writeln('Blocks:'),
    printMatrix(Blocks),
    solveKojun(OriginalBoard, Blocks, 0, 0, SolvedMatrix),
    nl, writeln('Solved:'), nl,
    printMatrix(SolvedMatrix), nl.