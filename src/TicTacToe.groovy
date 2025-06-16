// tic_tac_toe.groovy

def board = []

def initializeBoard = { ->
    board.clear()
    for (int i = 0; i < 9; i++) {
        board << (i + 1).toString()
    }
}

def displayBoard = { ->
    println "\n--- Tic-Tac-Toe Board ---"
    println "-------------"
    for (int i = 0; i < 9; i += 3) {
        printf "| %s | %s | %s |\n", board[i], board[i + 1], board[i + 2]
        println "-------------"
    }
    println "-------------------------\n"
}

def checkWin = { playerMark ->
    def winConditions = [
        [0, 1, 2], [3, 4, 5], [6, 7, 8],
        [0, 3, 6], [1, 4, 7], [2, 5, 8],
        [0, 4, 8], [2, 4, 6]
    ]

    for (condition in winConditions) {
        if (board[condition[0]] == playerMark &&
            board[condition[1]] == playerMark &&
            board[condition[2]] == playerMark) {
            return true
        }
    }
    return false
}

def checkDraw = { ->
    for (cell in board) {
        if (cell.isInteger()) {
            return false
        }
    }
    return true
}

def playGame = { ->
    initializeBoard()
    def currentPlayer = 'X'
    def gameOver = false

    println "Welcome to Tic-Tac-Toe!"
    println "Players use 'X' and 'O'. Enter the number of the cell (1-9) to place your mark."

    while (!gameOver) {
        displayBoard()

        def validMove = false
        def choice = -1

        while (!validMove) {
            print "Player ${currentPlayer}'s turn. Enter your move (1-9): "
            def input = System.console()?.readLine()

            if (!input) {
                println "Invalid input. Please enter a number from 1 to 9."
                continue
            }

            if (input.isInteger()) {
                choice = input.toInteger()
                def index = choice - 1

                if (index >= 0 && index < 9 && board[index].isInteger()) {
                    board[index] = currentPlayer
                    validMove = true
                } else if (index < 0 || index >= 9) {
                    println "Invalid choice. Please enter a number from 1 to 9."
                } else {
                    println "Invalid move. That cell is already taken. Please choose an empty cell."
                }
            } else {
                println "Invalid input. Please enter a number from 1 to 9."
            }
        }

        if (checkWin(currentPlayer)) {
            displayBoard()
            println "Player ${currentPlayer} wins! Congratulations!"
            gameOver = true
        } else if (checkDraw()) {
            displayBoard()
            println "It's a draw!"
            gameOver = true
        } else {
            currentPlayer = (currentPlayer == 'X') ? 'O' : 'X'
        }
    }

    println "Game over. Thanks for playing!"
}

playGame()

