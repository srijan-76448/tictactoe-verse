--!/usr/bin/env lua

function create_board()
    local board = {}
    for i = 1, 3 do
        board[i] = {" ", " ", " "}
    end
    return board
end

function display_board(board)
    print("\n--- Tic-Tac-Toe Board ---")
    for i = 1, 3 do
        print("-------------")
        if i == 1 then
            print("-------------")
        end
        print("| " .. board[i][1] .. " | " .. board[i][2] .. " | " .. board[i][3] .. " |")
        print("-------------")
    end
    print("-------------------------\n")
end

function check_win(board, player)
    for i = 1, 3 do
        if board[i][1] == player and board[i][2] == player and board[i][3] == player then
            return true
        end
    end
    for j = 1, 3 do
        if board[1][j] == player and board[2][j] == player and board[3][j] == player then
            return true
        end
    end
    if (board[1][1] == player and board[2][2] == player and board[3][3] == player) then
        return true
    end
    if (board[1][3] == player and board[2][2] == player and board[3][1] == player) then
        return true
    end
    return false
end

function check_draw(board)
    for i = 1, 3 do
        for j = 1, 3 do
            if board[i][j] == " " then
                return false
            end
        end
    end
    return true
end

function play_game()
    local board = create_board()
    local current_player = "X"
    local game_over = false

    print("Welcome to Tic-Tac-Toe!")
    print("Players use 'X' and 'O'. Enter row and column (e.g., 1 2 for Row 1, Column 2).")

    while not game_over do
        display_board(board)
        print("Player " .. current_player .. "'s turn.")
        io.write("Enter your move (row col): ")

        local input = io.read("*l")
        local row_str, col_str = input:match("^(%d+)%s*(%d+)$")

        local row = tonumber(row_str)
        local col = tonumber(col_str)

        if row and col and row >= 1 and row <= 3 and col >= 1 and col <= 3 and board[row][col] == " " then
            board[row][col] = current_player

            if check_win(board, current_player) then
                display_board(board)
                print("Player " .. current_player .. " wins! Congratulations!")
                game_over = true
            elseif check_draw(board) then
                display_board(board)
                print("It's a draw!")
                game_over = true
            else
                if current_player == "X" then
                    current_player = "O"
                else
                    current_player = "X"
                end
            end
        else
            print("Invalid move. Please enter valid row and column numbers (1-3) for an empty cell.")
        end
    end
    print("Game over. Thanks for playing!")
end

play_game()

