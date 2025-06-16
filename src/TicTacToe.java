import java.util.Scanner;

public class TicTacToe {
    private static char[] board = new char[9];
    private static Scanner scanner = new Scanner(System.in);

    public static void initializeBoard() {
        for (int i = 0; i < 9; i++) {
            board[i] = (char) ('1' + i);
        }
    }

    public static void displayBoard() {
        System.out.println("\n--- Tic-Tac-Toe Board ---");
        System.out.println("-------------");
        for (int i = 0; i < 9; i += 3) {
            System.out.printf("| %c | %c | %c |\n", board[i], board[i + 1], board[i + 2]);
            System.out.println("-------------");
        }
        System.out.println("-------------------------\n");
    }

    public static boolean checkWin(char playerMark) {
        int[][] winConditions = {
            {0, 1, 2}, {3, 4, 5}, {6, 7, 8},
            {0, 3, 6}, {1, 4, 7}, {2, 5, 8},
            {0, 4, 8}, {2, 4, 6}
        };

        for (int[] condition : winConditions) {
            if (board[condition[0]] == playerMark &&
                board[condition[1]] == playerMark &&
                board[condition[2]] == playerMark) {
                return true;
            }
        }
        return false;
    }

    public static boolean checkDraw() {
        for (char cell : board) {
            if (Character.isDigit(cell)) {
                return false;
            }
        }
        return true;
    }

    public static void main(String[] args) {
        initializeBoard();
        char currentPlayer = 'X';
        boolean gameOver = false;

        System.out.println("Welcome to Tic-Tac-Toe!");
        System.out.println("Players use 'X' and 'O'. Enter the number of the cell (1-9) to place your mark.");

        while (!gameOver) {
            displayBoard();

            int choice = -1;
            boolean validMove = false;

            while (!validMove) {
                System.out.print("Player " + currentPlayer + "'s turn. Enter your move (1-9): ");

                if (scanner.hasNextInt()) {
                    choice = scanner.nextInt();
                    int index = choice - 1;

                    if (index >= 0 && index < 9 && Character.isDigit(board[index])) {
                        board[index] = currentPlayer;
                        validMove = true;
                    } else if (index < 0 || index >= 9) {
                        System.out.println("Invalid choice. Please enter a number from 1 to 9.");
                    } else {
                        System.out.println("Invalid move. That cell is already taken. Please choose an empty cell.");
                    }
                } else {
                    System.out.println("Invalid input. Please enter a number from 1 to 9.");
                    scanner.next();
                }
            }

            if (checkWin(currentPlayer)) {
                displayBoard();
                System.out.println("Player " + currentPlayer + " wins! Congratulations!");
                gameOver = true;
            } else if (checkDraw()) {
                displayBoard();
                System.out.println("It's a draw!");
                gameOver = true;
            } else {
                currentPlayer = (currentPlayer == 'X') ? 'O' : 'X';
            }
        }

        System.out.println("Game over. Thanks for playing!");
        scanner.close();
    }
}
