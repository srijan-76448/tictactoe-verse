#!/bin/bash

declare -a board

initialize_board() {
    for i in {0..8}; do
        board[$i]=$((i + 1))
    done
}

display_board() {
    echo -e "\n--- Tic-Tac-Toe Board ---"
    echo "-------------"
    echo "| ${board[0]} | ${board[1]} | ${board[2]} |"
    echo "-------------"
    echo "| ${board[3]} | ${board[4]} | ${board[5]} |"
    echo "-------------"
    echo "| ${board[6]} | ${board[7]} | ${board[8]} |"
    echo "-------------"
    echo -e "-------------------------\n"
}

check_win() {
    local player_mark=$1
    for i in 0 3 6; do
        if [[ "${board[$i]}" == "${player_mark}" && \
              "${board[$((i+1))]}" == "${player_mark}" && \
              "${board[$((i+2))]}" == "${player_mark}" ]]; then
            return 0
        fi
    done
    for i in 0 1 2; do
        if [[ "${board[$i]}" == "${player_mark}" && \
              "${board[$((i+3))]}" == "${player_mark}" && \
              "${board[$((i+6))]}" == "${player_mark}" ]]; then
            return 0
        fi
    done
    if [[ "${board[0]}" == "${player_mark}" && \
          "${board[4]}" == "${player_mark}" && \
          "${board[8]}" == "${player_mark}" ]]; then
        return 0
    fi
    if [[ "${board[2]}" == "${player_mark}" && \
          "${board[4]}" == "${player_mark}" && \
          "${board[6]}" == "${player_mark}" ]]; then
        return 0
    fi
    return 1
}

check_draw() {
    for i in {0..8}; do
        if [[ "${board[$i]}" =~ ^[0-9]$ ]]; then
            return 1
        fi
    done
    return 0
}

game_loop() {
    local current_player="X"
    local game_over=0
    local turn_count=0
    echo "Welcome to Tic-Tac-Toe!"
    echo "Players use 'X' and 'O'. Enter the number of the cell (1-9) to place your mark."
    while [[ ${game_over} -eq 0 ]]; do
        display_board
        local choice
        local valid_move=0
        while [[ ${valid_move} -eq 0 ]]; do
            read -p "Player ${current_player}'s turn. Enter your move (1-9): " choice
            if [[ "${choice}" =~ ^[1-9]$ ]]; then
                local index=$((choice - 1))
                if [[ "${board[${index}]}" =~ ^[0-9]$ ]]; then
                    board[${index}]="${current_player}"
                    valid_move=1
                else
                    echo "Invalid move. That cell is already taken."
                fi
            else
                echo "Invalid input. Please enter a number from 1-9."
            fi
        done
        turn_count=$((turn_count + 1))
        if check_win "${current_player}"; then
            display_board
            echo "Player ${current_player} wins! Congratulations!"
            game_over=1
        elif [[ ${turn_count} -eq 9 ]]; then
            if check_draw; then
                display_board
                echo "It's a draw!"
                game_over=1
            fi
        fi
        if [[ ${game_over} -eq 0 ]]; then
            if [[ "${current_player}" == "X" ]]; then
                current_player="O"
            else
                current_player="X"
            fi
        fi
    done
    echo "Game over. Thanks for playing!"
}

initialize_board
game_loop
