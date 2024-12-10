:- use_module(library(clpfd)).
:- consult('puzzles.pl').

board(ID, Board) :-
    puzzle(ID, Board, _).

blocks(ID, Blocks) :-
    puzzle(ID, _, Blocks).

size(Block, Size) :-
    length(Block, Size).

printList([]).
printList([H|T]) :- write(H), write(" "), printList(T).

printMatrix([]).
printMatrix([H|T]) :- printList(H), nl, printMatrix(T).

get_position(Board, I, J, Found) :- 
    nth0(I, Board, Line),
    nth0(J, Line, Found).

set_position(Position, Value, List, NewList) :- 
    length(Prefix, Position),
    append(Prefix, [_|Suffix], List),
    append(Prefix, [Value|Suffix], NewList).

update_board(Board, I, J, Value, Solution) :-
    nth0(I, Board, OldRow, RestRows),
    set_position(J, Value, OldRow, NewRow),
    nth0(I, Solution, NewRow, RestRows).

find_number_on_block(Board, Blocks, BlockId, Result) :-
    get_position(Blocks, I, J, BlockId),
    get_position(Board, I, J, Result).

block_numbers(Board, Blocks, BlockId, FinalList) :- 
    findall(FoundNumber, find_number_on_block(Board, Blocks, BlockId, FoundNumber), FinalList).

complement(Board, List, Complement) :-
    size(Board, Max),
    numlist(1, Max, UniverseList), 
    delete(List, 0, FilteredList),
    subtract(UniverseList, FilteredList, Complement).

is_empty_cell(Board, Blocks, BlockId, [I, J]) :-
    get_position(Blocks, I, J, BlockId),
    get_position(Board, I, J, 0).

get_empty_cells(Board, Blocks, BlockId, ZeroedPositions) :- 
    findall([I, J], is_empty_cell(Board, Blocks, BlockId, [I, J]), ZeroedPositions).

verify_empty(Board, Blocks, I, J) :-
    get_position(Blocks, I, J, BlockId),
    get_empty_cells(Board, Blocks, BlockId, ListOfZeroedPositions),
    member([I, J], ListOfZeroedPositions).

get_possibilities(Board, Blocks, I, J, PossibleValue) :-
    get_position(Blocks, I, J, BlockId),
    block_numbers(Board, Blocks, BlockId, RegionNumbers),
    complement(Board, RegionNumbers, Complement),
    member(PossibleValue, Complement).

verifyAboveGreater(BelowValue, AboveValue) :-
    AboveValue =:= 0 -> true; BelowValue < AboveValue.

verify_upper(Board, Blocks, I, J, Value, IAbove) :-
    get_position(Blocks, I, J, BlockId),
    get_position(Blocks, IAbove, J, AboveBlockId),
    BlockId =:= AboveBlockId ->
    get_position(Board, IAbove, J, AboveValue),
    verifyAboveGreater(Value, AboveValue);
    get_position(Board, IAbove, J, AboveValue),
    Value =\= AboveValue.

verify_bottom(Board, Blocks, I, J, Value, IBelow) :-
    get_position(Blocks, I, J, BlockId),
    get_position(Blocks, IBelow, J, IdBottomRegion),
    BlockId =:= IdBottomRegion ->
    get_position(Board, IBelow, J, BelowValue), Value > BelowValue; 
    get_position(Board, IBelow, J, BelowValue), Value =\= BelowValue.

is_upper_valid(Board, Blocks, I, J, PossibleValue) :-
    IAbove is (I - 1), IAbove >= 0 ->
    verify_upper(Board, Blocks, I, J, PossibleValue, IAbove); true.

is_bottom_valid(Board, Blocks, I, J, PossibleValue) :-
    length(Board, Length),
    IBelow is (I + 1), IBelow < Length ->
    verify_bottom(Board, Blocks, I, J, PossibleValue, IBelow); true.

is_left_valid(Board, I, J, Value) :-
    Left is (J - 1), Left >= 0 ->
    get_position(Board, I, Left, LeftValue),
    Value =\= LeftValue; true.

is_right_valid(Board, I, J, Value) :- 
    length(Board, Length),
    IRight is (J + 1), IRight < Length ->
    get_position(Board, I, IRight, RightValue),
    Value =\= RightValue; true.

fillPosition(Board, Blocks, I, J, Solution) :-
    verify_empty(Board, Blocks, I, J) ->
    get_possibilities(Board, Blocks, I, J, PossibleValue),
    is_upper_valid(Board, Blocks, I, J, PossibleValue),
    is_bottom_valid(Board, Blocks, I, J, PossibleValue),
    is_left_valid(Board, I, J, PossibleValue),
    is_right_valid(Board, I, J, PossibleValue),
    update_board(Board, I, J, PossibleValue, Solution);
    Solution = Board.

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
    writeln('Solved:'),
    printMatrix(SolvedMatrix), nl.