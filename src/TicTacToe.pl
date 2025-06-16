% tic_tac_toe.pl

initialize_board([1, 2, 3, 4, 5, 6, 7, 8, 9]).

display_board(Board) :-
    nl,
    writeln('--- Tic-Tac-Toe Board ---'),
    writeln('-------------'),
    nth0(0, Board, B0), nth0(1, Board, B1), nth0(2, Board, B2),
    format('| ~w | ~w | ~w |~n', [B0, B1, B2]),
    writeln('-------------'),
    nth0(3, Board, B3), nth0(4, Board, B4), nth0(5, Board, B5),
    format('| ~w | ~w | ~w |~n', [B3, B4, B5]),
    writeln('-------------'),
    nth0(6, Board, B6), nth0(7, Board, B7), nth0(8, Board, B8),
    format('| ~w | ~w | ~w |~n', [B6, B7, B8]),
    writeln('-------------'),
    writeln('-------------------------\n').

make_move(OldBoard, Player, Choice, NewBoard) :-
    integer(Choice),
    Choice >= 1, Choice =< 9,
    Pos is Choice - 1,
    nth0(Pos, OldBoard, CurrentCell),
    integer(CurrentCell),
    replace_nth(OldBoard, Pos, Player, NewBoard).

replace_nth([_|T], 0, NewElement, [NewElement|T]).
replace_nth([H|T], Index, NewElement, [H|NewList]) :-
    Index > 0,
    NextIndex is Index - 1,
    replace_nth(T, NextIndex, NewElement, NewList).

check_win(Board, Player) :-
    nth0(0, Board, Player), nth0(1, Board, Player), nth0(2, Board, Player).
check_win(Board, Player) :-
    nth0(3, Board, Player), nth0(4, Board, Player), nth0(5, Board, Player).
check_win(Board, Player) :-
    nth0(6, Board, Player), nth0(7, Board, Player), nth0(8, Board, Player).

check_win(Board, Player) :-
    nth0(0, Board, Player), nth0(3, Board, Player), nth0(6, Board, Player).
check_win(Board, Player) :-
    nth0(1, Board, Player), nth0(4, Board, Player), nth0(7, Board, Player).
check_win(Board, Player) :-
    nth0(2, Board, Player), nth0(5, Board, Player), nth0(8, Board, Player).

check_win(Board, Player) :-
    nth0(0, Board, Player), nth0(4, Board, Player), nth0(8, Board, Player).
check_win(Board, Player) :-
    nth0(2, Board, Player), nth0(4, Board, Player), nth0(6, Board, Player).

check_draw(Board) :-
    \+ (member(Cell, Board), integer(Cell)).

get_player_choice(Choice) :-
    write('Enter your move (1-9): '),
    flush_output,
    read_line_to_string(user_input, InputString),
    (   atom_string(Atom, InputString),
        atom_number(Atom, Choice)
    ->  true
    ;   fail
    ).

game_over(Board, CurrentPlayer) :-
    check_win(Board, CurrentPlayer),
    display_board(Board),
    format('Player ~w wins! Congratulations!~n', [CurrentPlayer]),
    !.

game_over(Board, _) :-
    check_draw(Board),
    display_board(Board),
    writeln('It''s a draw!'),
    !.

play_game(Board, CurrentPlayer) :-
    game_over(Board, CurrentPlayer),
    writeln('Game over. Thanks for playing!').

play_game(Board, CurrentPlayer) :-
    display_board(Board),
    format('Player ~w''s turn.~n', [CurrentPlayer]),
    (   get_player_choice(Choice),
        make_move(Board, CurrentPlayer, Choice, NewBoard)
    ->  (CurrentPlayer = x -> NextPlayer = o ; NextPlayer = x),
        play_game(NewBoard, NextPlayer)
    ;   writeln('Invalid move. Please enter a number from 1-9 for an empty cell.'),
        play_game(Board, CurrentPlayer)
    ).

start_game :-
    writeln('Welcome to Tic-Tac-Toe!'),
    writeln('Players use ''x'' and ''o''. Enter the number of the cell (1-9) to place your mark.'),
    initialize_board(InitialBoard),
    play_game(InitialBoard, x).

