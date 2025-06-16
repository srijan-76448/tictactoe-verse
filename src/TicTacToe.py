#!/usr/bin/python3

def print_board(board):
    """Prints the current state of the Tic-Tac-Toe board."""
    for i, row in enumerate(board):
        print("|".join(row))
        if i < 2:
            print("-----")

def check_win(board, player):
    """Checks if the given player has won the game."""
    # Check rows
    for row in board:
        if all(s == player for s in row):
            return True
    # Check columns
    for col in range(3):
        if all(board[row][col] == player for row in range(3)):
            return True
    # Check diagonals
    if all(board[i][i] == player for i in range(3)): # Main diagonal
        return True
    if all(board[i][2 - i] == player for i in range(3)): # Anti-diagonal
        return True
    return False

def is_board_full(board):
    """Checks if the board is completely filled."""
    for row in board:
        for cell in row:
            if cell == ' ':
                return False
    return True

def tic_tac_toe():
    """Main function to run the Tic-Tac-Toe game."""
    board = [[' ' for _ in range(3)] for _ in range(3)]
    players = ['X', 'O']
    current_player_idx = 0

    print("Welcome to Tic-Tac-Toe!")
    print_board(board)

    while True:
        player = players[current_player_idx]
        print(f"\nPlayer {player}'s turn.")

        try:
            row = int(input("Enter row (0, 1, 2): "))
            col = int(input("Enter column (0, 1, 2): "))
        except ValueError:
            print("Invalid input. Please enter a number between 0 and 2.")
            continue

        if not (0 <= row <= 2 and 0 <= col <= 2):
            print("Row or column out of bounds. Please enter values between 0 and 2.")
            continue

        if board[row][col] == ' ':
            board[row][col] = player
        else:
            print("That cell is already taken. Please choose an empty cell.")
            continue

        print_board(board)

        if check_win(board, player):
            print(f"Player {player} wins! Congratulations!")
            break
        elif is_board_full(board):
            print("It's a draw! The board is full.")
            break

        current_player_idx = 1 - current_player_idx # Switch player (0 to 1, 1 to 0)

if __name__ == "__main__":
    tic_tac_toe()
