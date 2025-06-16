#include <iostream>
#include <vector>
#include <string>

void initialize_board(std::vector<char>& board) {
    board.resize(9);
    for (int i = 0; i < 9; ++i) {
        board[i] = (char)('1' + i);
    }
}

void display_board(const std::vector<char>& board) {
    std::cout << "\n--- Tic-Tac-Toe Board ---\n";
    std::cout << "-------------\n";
    for (int i = 0; i < 9; ++i) {
        std::cout << "| " << board[i] << " ";
        if ((i + 1) % 3 == 0) {
            std::cout << "|\n";
            std::cout << "-------------\n";
        }
    }
    std::cout << "-------------------------\n\n";
}

bool check_win(const std::vector<char>& board, char player) {
    for (int i = 0; i < 9; i += 3) {
        if (board[i] == player && board[i+1] == player && board[i+2] == player) {
            return true;
        }
    }
    for (int i = 0; i < 3; ++i) {
        if (board[i] == player && board[i+3] == player && board[i+6] == player) {
            return true;
        }
    }
    if (board[0] == player && board[4] == player && board[8] == player) {
        return true;
    }
    if (board[2] == player && board[4] == player && board[6] == player) {
        return true;
    }
    return false;
}

bool check_draw(const std::vector<char>& board) {
    for (int i = 0; i < 9; ++i) {
        if (board[i] >= '1' && board[i] <= '9') {
            return false;
        }
    }
    return true;
}

int main() {
    std::vector<char> board;
    initialize_board(board);
    char current_player = 'X';
    bool game_over = false;

    std::cout << "Welcome to Tic-Tac-Toe!\n";
    std::cout << "Players use 'X' and 'O'. Enter the number of the cell (1-9) to place your mark.\n";

    while (!game_over) {
        display_board(board);

        int choice;
        bool valid_move = false;

        while (!valid_move) {
            std::cout << "Player " << current_player << "'s turn. Enter your move: ";
            std::cin >> choice;

            if (choice >= 1 && choice <= 9 && board[choice - 1] >= '1' && board[choice - 1] <= '9') {
                board[choice - 1] = current_player;
                valid_move = true;
            } else {
                std::cout << "Invalid move. Please enter a number from 1-9 for an empty cell.\n";
                std::cin.clear();
                std::cin.ignore(10000, '\n');
            }
        }

        if (check_win(board, current_player)) {
            display_board(board);
            std::cout << "Player " << current_player << " wins! Congratulations!\n";
            game_over = true;
        } else if (check_draw(board)) {
            display_board(board);
            std::cout << "It's a draw!\n";
            game_over = true;
        } else {
            current_player = (current_player == 'X') ? 'O' : 'X';
        }
    }

    std::cout << "Game over. Thanks for playing!\n";
    return 0;
}
