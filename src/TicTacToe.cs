using System;

public class TicTacToe
{
    private static char[] board = new char[9];

    private static void InitializeBoard()
    {
        for (int i = 0; i < 9; i++)
        {
            board[i] = (char)('1' + i);
        }
    }

    private static void DisplayBoard()
    {
        Console.WriteLine("\n--- Tic-Tac-Toe Board ---");
        Console.WriteLine("-------------");
        for (int i = 0; i < 9; i += 3)
        {
            Console.WriteLine($"| {board[i]} | {board[i + 1]} | {board[i + 2]} |");
            Console.WriteLine("-------------");
        }
        Console.WriteLine("-------------------------\n");
    }

    private static bool CheckWin(char playerMark)
    {
        int[][] winConditions = new int[][]
        {
            new int[] {0, 1, 2}, new int[] {3, 4, 5}, new int[] {6, 7, 8},
            new int[] {0, 3, 6}, new int[] {1, 4, 7}, new int[] {2, 5, 8},
            new int[] {0, 4, 8}, new int[] {2, 4, 6}
        };

        foreach (var condition in winConditions)
        {
            if (board[condition[0]] == playerMark &&
                board[condition[1]] == playerMark &&
                board[condition[2]] == playerMark)
            {
                return true;
            }
        }
        return false;
    }

    private static bool CheckDraw()
    {
        foreach (char cell in board)
        {
            if (char.IsDigit(cell))
            {
                return false;
            }
        }
        return true;
    }

    public static void Main(string[] args)
    {
        InitializeBoard();
        char currentPlayer = 'X';
        bool gameOver = false;

        Console.WriteLine("Welcome to Tic-Tac-Toe!");
        Console.WriteLine("Players use 'X' and 'O'. Enter the number of the cell (1-9) to place your mark.");

        while (!gameOver)
        {
            DisplayBoard();

            int choice = -1;
            bool validMove = false;

            while (!validMove)
            {
                Console.Write($"Player {currentPlayer}'s turn. Enter your move (1-9): ");
                string? input = Console.ReadLine();

                if (int.TryParse(input, out choice))
                {
                    int index = choice - 1;

                    if (index >= 0 && index < 9 && char.IsDigit(board[index]))
                    {
                        board[index] = currentPlayer;
                        validMove = true;
                    }
                    else if (index < 0 || index >= 9)
                    {
                        Console.WriteLine("Invalid choice. Please enter a number from 1 to 9.");
                    }
                    else
                    {
                        Console.WriteLine("Invalid move. That cell is already taken. Please choose an empty cell.");
                    }
                }
                else
                {
                    Console.WriteLine("Invalid input. Please enter a number from 1 to 9.");
                }
            }

            if (CheckWin(currentPlayer))
            {
                DisplayBoard();
                Console.WriteLine($"Player {currentPlayer} wins! Congratulations!");
                gameOver = true;
            }
            else if (CheckDraw())
            {
                DisplayBoard();
                Console.WriteLine("It's a draw!");
                gameOver = true;
            }
            else
            {
                currentPlayer = (currentPlayer == 'X') ? 'O' : 'X';
            }
        }

        Console.WriteLine("Game over. Thanks for playing!");
    }
}
