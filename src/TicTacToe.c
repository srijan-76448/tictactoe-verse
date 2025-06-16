#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char board[9];

void clear_screen() {
#ifdef _WIN32
    system("cls");
#else
    system("clear");
#endif
}

void initialize_board() {
    for (int i = 0; i < 9; ++i) {
        board[i] = (char)('1' + i);
    }
}

void display_board() {
    clear_screen();
    printf("\n--- Tic-Tac-Toe Board ---\n");
    printf("-------------\n");
    for (int i = 0; i < 9; i += 3) {
        printf("| %c | %c | %c |\n", board[i], board[i+1], board[i+2]);
        printf("-------------\n");
    }
    printf("-------------------------\n\n");
}

int check_win(char player_mark) {
    int win_conditions[8][3] = {
        {0, 1, 2}, {3, 4, 5}, {6, 7, 8},
        {0, 3, 6}, {1, 4, 7}, {2, 5, 8},
        {0, 4, 8}, {2, 4, 6}
    };

    for (int i = 0; i < 8; ++i) {
        if (board[win_conditions[i][0]] == player_mark &&
            board[win_conditions[i][1]] == player_mark &&
            board[win_conditions[i][2]] == player_mark) {
            return 1;
        }
    }
    return 0;
}

int check_draw() {
    for (int i = 0; i < 9; ++i) {
        if (board[i] >= '1' && board[i] <= '9') {
            return 0;
        }
    }
    return 1;
}

int main() {
    initialize_board();
    char current_player = 'X';
    int game_over = 0;
    int choice;

    printf("Welcome to Tic-Tac-Toe!\n");
    printf("Players use 'X' and 'O'. Enter the number of the cell (1-9) to place your mark.\n");

    while (!game_over) {
        display_board();

        int valid_move = 0;

        while (!valid_move) {
            printf("Player %c's turn. Enter your move (1-9): ", current_player);

            while (scanf("%d", &choice) != 1) {
                printf("Invalid input. Please enter a number from 1 to 9: ");
                int c;
                while ((c = getchar()) != '\n' && c != EOF);
            }

            int c;
            while ((c = getchar()) != '\n' && c != EOF);

            int index = choice - 1;

            if (index >= 0 && index < 9 && board[index] >= '1' && board[index] <= '9') {
                board[index] = current_player;
                valid_move = 1;
            } else {
                printf("Invalid move. Please enter a number from 1-9 for an empty cell.\n");
            }
        }

        if (check_win(current_player)) {
            display_board();
            printf("Player %c wins! Congratulations!\n", current_player);
            game_over = 1;
        } else if (check_draw()) {
            display_board();
            printf("It's a draw!\n");
            game_over = 1;
        } else {
            current_player = (current_player == 'X') ? 'O' : 'X';
        }
    }

    printf("Game over. Thanks for playing!\n");
    return 0;
}
