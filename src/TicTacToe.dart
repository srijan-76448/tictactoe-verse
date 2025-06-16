import 'dart:io';

List<String> board = List<String>.filled(9, '');

void initializeBoard() {
  for (int i = 0; i < 9; i++) {
    board[i] = (i + 1).toString();
  }
}

void displayBoard() {
  print("\n--- Tic-Tac-Toe Board ---");
  print("-------------");
  for (int i = 0; i < 9; i += 3) {
    print("| ${board[i]} | ${board[i + 1]} | ${board[i + 2]} |");
    print("-------------");
  }
  print("-------------------------\n");
}

bool checkWin(String playerMark) {
  List<List<int>> winConditions = [
    [0, 1, 2], [3, 4, 5], [6, 7, 8],
    [0, 3, 6], [1, 4, 7], [2, 5, 8],
    [0, 4, 8], [2, 4, 6]
  ];
  for (var condition in winConditions) {
    if (board[condition[0]] == playerMark &&
        board[condition[1]] == playerMark &&
        board[condition[2]] == playerMark) {
      return true;
    }
  }
  return false;
}

bool checkDraw() {
  for (String cell in board) {
    if (int.tryParse(cell) != null) {
      return false;
    }
  }
  return true;
}

void main() {
  initializeBoard();
  String currentPlayer = 'X';
  bool gameOver = false;

  print("Welcome to Tic-Tac-Toe!");
  print("Players use 'X' and 'O'. Enter the number of the cell (1-9) to place your mark.");

  while (!gameOver) {
    displayBoard();

    int? choice;
    bool validMove = false;

    while (!validMove) {
      stdout.write("Player $currentPlayer's turn. Enter your move (1-9): ");
      String? input = stdin.readLineSync();

      if (input == null || input.isEmpty) {
        print("Invalid input. Please enter a number from 1 to 9.");
        continue;
      }

      choice = int.tryParse(input);

      if (choice != null) {
        int index = choice - 1;

        if (index >= 0 && index < 9 && int.tryParse(board[index]) != null) {
          board[index] = currentPlayer;
          validMove = true;
        } else if (index < 0 || index >= 9) {
          print("Invalid choice. Please enter a number from 1 to 9.");
        } else {
          print("Invalid move. That cell is already taken. Please choose an empty cell.");
        }
      } else {
        print("Invalid input. Please enter a number from 1 to 9.");
      }
    }

    if (checkWin(currentPlayer)) {
      displayBoard();
      print("Player $currentPlayer wins! Congratulations!");
      gameOver = true;
    } else if (checkDraw()) {
      displayBoard();
      print("It's a draw!");
      gameOver = true;
    } else {
      currentPlayer = (currentPlayer == 'X') ? 'O' : 'X';
    }
  }

  print("Game over. Thanks for playing!");
}

