package main

import (
	"fmt"
	"strconv"
	"strings"
)

var board [9]string

func initializeBoard() {
	for i := 0; i < 9; i++ {
		board[i] = strconv.Itoa(i + 1)
	}
}

func displayBoard() {
	fmt.Println("\n--- Tic-Tac-Toe Board ---")
	fmt.Println("-------------")
	for i := 0; i < 9; i += 3 {
		fmt.Printf("| %s | %s | %s |\n", board[i], board[i+1], board[i+2])
		fmt.Println("-------------")
	}
	fmt.Println("-------------------------\n")
}

func checkWin(playerMark string) bool {
	winConditions := [][]int{
		{0, 1, 2}, {3, 4, 5}, {6, 7, 8},
		{0, 3, 6}, {1, 4, 7}, {2, 5, 8},
		{0, 4, 8}, {2, 4, 6},
	}

	for _, condition := range winConditions {
		if board[condition[0]] == playerMark &&
			board[condition[1]] == playerMark &&
			board[condition[2]] == playerMark {
			return true
		}
	}
	return false
}

func checkDraw() bool {
	for i := 0; i < 9; i++ {
		if _, err := strconv.Atoi(board[i]); err == nil {
			return false
		}
	}
	return true
}

func main() {
	initializeBoard()
	currentPlayer := "X"
	gameOver := false

	fmt.Println("Welcome to Tic-Tac-Toe!")
	fmt.Println("Players use 'X' and 'O'. Enter the number of the cell (1-9) to place your mark.")

	for !gameOver {
		displayBoard()

		var choice int
		validMove := false

		for !validMove {
			fmt.Printf("Player %s's turn. Enter your move (1-9): ", currentPlayer)
			_, err := fmt.Scanln(&choice)

			if err != nil {
				fmt.Println("Invalid input. Please enter a number.")
				var discard string
				fmt.Scanln(&discard)
				continue
			}

			index := choice - 1

			if index >= 0 && index < 9 {
				if _, err := strconv.Atoi(board[index]); err == nil {
					board[index] = currentPlayer
					validMove = true
				} else {
					fmt.Println("Invalid move. That cell is already taken. Please choose an empty cell.")
				}
			} else {
				fmt.Println("Invalid choice. Please enter a number from 1 to 9.")
			}
		}

		if checkWin(currentPlayer) {
			displayBoard()
			fmt.Printf("Player %s wins! Congratulations!\n", currentPlayer)
			gameOver = true
		} else if checkDraw() {
			displayBoard()
			fmt.Println("It's a draw!")
			gameOver = true
		} else {
			if currentPlayer == "X" {
				currentPlayer = "O"
			} else {
				currentPlayer = "X"
			}
		}
	}

	fmt.Println("Game over. Thanks for playing!")
}

