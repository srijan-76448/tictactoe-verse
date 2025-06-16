-module(tic_tac_toe).
-export([start/0, play_game/2]).

start() ->
    io:format("Welcome to Tic-Tac-Toe!~n"),
    io:format("Players use 'x' and 'o'. Enter the number of the cell (1-9) to place your mark.~n"),
    Board = initialize_board(),
    play_game(Board, x).

initialize_board() ->
    lists:seq(1, 9).

display_board(Board) ->
    io:format("~n--- Tic-Tac-Toe Board ---~n"),
    io:format("-------------~n"),
    [B1,B2,B3,B4,B5,B6,B7,B8,B9] = Board,
    io:format("| ~w | ~w | ~w |~n", [B1, B2, B3]),
    io:format("-------------~n"),
    io:format("| ~w | ~w | ~w |~n", [B4, B5, B6]),
    io:format("-------------~n"),
    io:format("| ~w | ~w | ~w |~n", [B7, B8, B9]),
    io:format("-------------~n"),
    io:format("-------------------------~n~n").

make_move(OldBoard, Player, Choice, NewBoard) when is_integer(Choice), Choice >= 1, Choice =< 9 ->
    Pos = Choice - 1,
    case lists:nth(Choice, OldBoard) of
        Cell when is_integer(Cell) ->
            NewBoard = lists:replace_nth(Choice, Player, OldBoard),
            {ok, NewBoard};
        _ ->
            {error, taken}
    end;
make_move(_OldBoard, _Player, _Choice, _NewBoard) ->
    {error, invalid_input}.

check_win(Board, Player) ->
    [B1,B2,B3,B4,B5,B6,B7,B8,B9] = Board,
    (B1 == Player andalso B2 == Player andalso B3 == Player) orelse
    (B4 == Player andalso B5 == Player andalso B6 == Player) orelse
    (B7 == Player andalso B8 == Player andalso B9 == Player) orelse
    (B1 == Player andalso B4 == Player andalso B7 == Player) orelse
    (B2 == Player andalso B5 == Player andalso B8 == Player) orelse
    (B3 == Player andalso B6 == Player andalso B9 == Player) orelse
    (B1 == Player andalso B5 == Player andalso B9 == Player) orelse
    (B3 == Player andalso B5 == Player andalso B7 == Player).

check_draw(Board) ->
    not lists:member_fun(fun is_integer/1, Board).

get_player_choice() ->
    io:format("Enter your move (1-9): "),
    Line = io:get_line(""),
    case string:to_integer(string:trim(Line)) of
        {ok, Choice, _} -> Choice;
        _ -> 0
    end.

play_game(Board, CurrentPlayer) ->
    display_board(Board),
    case check_win(Board, CurrentPlayer) of
        true ->
            io:format("Player ~w wins! Congratulations!~n", [CurrentPlayer]),
            io:format("Game over. Thanks for playing!~n");
        false ->
            case check_draw(Board) of
                true ->
                    io:format("It's a draw!~n"),
                    io:format("Game over. Thanks for playing!~n");
                false ->
                    io:format("Player ~w's turn.~n", [CurrentPlayer]),
                    Choice = get_player_choice(),
                    case make_move(Board, CurrentPlayer, Choice) of
                        {ok, NewBoard} ->
                            NextPlayer = case CurrentPlayer of
                                             x -> o;
                                             o -> x
                                         end,
                            play_game(NewBoard, NextPlayer);
                        {error, taken} ->
                            io:format("Invalid move. That cell is already taken. Please choose an empty cell.~n"),
                            play_game(Board, CurrentPlayer);
                        {error, invalid_input} ->
                            io:format("Invalid input. Please enter a number from 1-9 for an empty cell.~n"),
                            play_game(Board, CurrentPlayer)
                    end
            end
    end.
