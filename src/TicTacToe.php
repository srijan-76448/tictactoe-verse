<?php
// tic_tac_toe.php

$board = [];

function initialize_board() {
    global $board;
    for ($i = 0; $i < 9; $i++) {
        $board[$i] = (string)($i + 1);
    }
}

function display_board() {
    global $board;
    echo "\n--- Tic-Tac-Toe Board ---\n";
    echo "-------------\n";
    for ($i = 0; $i < 9; $i += 3) {
        printf("| %s | %s | %s |\n", $board[$i], $board[$i+1], $board[$i+2]);
        echo "-------------\n";
    }
    echo "-------------------------\n\n";
}

function check_win($player_mark) {
    global $board;
    $win_conditions = [
        [0, 1, 2], [3, 4, 5], [6, 7, 8],
        [0, 3, 6], [1, 4, 7], [2, 5, 8],
        [0, 4, 8], [2, 4, 6]
    ];

    foreach ($win_conditions as $condition) {
        if ($board[$condition[0]] == $player_mark &&
            $board[$condition[1]] == $player_mark &&
            $board[$condition[2]] == $player_mark) {
            return true;
        }
    }
    return false;
}

function check_draw() {
    global $board;
    foreach ($board as $cell) {
        if (is_numeric($cell)) {
            return false;
        }
    }
    return true;
}

function play_game() {
    global $board;
    initialize_board();
    $current_player = 'X';
    $game_over = false;

    echo "Welcome to Tic-Tac-Toe!\n";
    echo "Players use 'X' and 'O'. Enter the number of the cell (1-9) to place your mark.\n";

    while (!$game_over) {
        display_board();

        $valid_move = false;
        $choice = null;

        while (!$valid_move) {
            echo "Player {$current_player}'s turn. Enter your move (1-9): ";
            $input = trim(fgets(STDIN));

            if (is_numeric($input) && $input >= 1 && $input <= 9) {
                $choice = (int)$input;
                $index = $choice - 1;

                if (is_numeric($board[$index])) {
                    $board[$index] = $current_player;
                    $valid_move = true;
                } else {
                    echo "Invalid move. That cell is already taken. Please choose an empty cell.\n";
                }
            } else {
                echo "Invalid input. Please enter a number from 1 to 9.\n";
            }
        }

        if (check_win($current_player)) {
            display_board();
            echo "Player {$current_player} wins! Congratulations!\n";
            $game_over = true;
        } else if (check_draw()) {
            display_board();
            echo "It's a draw!\n";
            $game_over = true;
        } else {
            $current_player = ($current_player == 'X') ? 'O' : 'X';
        }
    }

    echo "Game over. Thanks for playing!\n";
}

play_game();

