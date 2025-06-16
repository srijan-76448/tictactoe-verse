import std/strutils
import std/sequtils
import std/io

var board: seq[string]

proc initializeBoard() =
  board = newSeq[string](9)
  for i in 0..<9:
    board[i] = $(i + 1)

proc displayBoard() =
  echo "\n--- Tic-Tac-Toe Board ---"
  echo "-------------"
  for i in 0..<3:
    let row_start_index = i * 3
    echo "| " & board[row_start_index] & " | " & board[row_start_index + 1] & " | " & board[row_start_index + 2] & " |"
    echo "-------------"
  echo "-------------------------\n"

proc checkWin(playerMark: string): bool =
  let winConditions = [
    @[0, 1, 2], @[3, 4, 5], @[6, 7, 8],
    @[0, 3, 6], @[1, 4, 7], @[2, 5, 8],
    @[0, 4, 8], @[2, 4, 6]
  ]
  for condition in winConditions:
    if board[condition[0]] == playerMark and
       board[condition[1]] == playerMark and
       board[condition[2]] == playerMark:
      return true
  return false

proc checkDraw(): bool =
  for cell in board:
    if cell.isDigit():
      return false
  return true

proc playGame() =
  var currentPlayer = "X"
  var gameOver = false
  echo "Welcome to Tic-Tac-Toe!"
  echo "Players use 'X' and 'O'. Enter the number of the cell (1-9) to place your mark."
  while not gameOver:
    displayBoard()
    var validMove = false
    var choice: int
    while not validMove:
      stdout.write("Player " & currentPlayer & "'s turn. Enter your move (1-9): ")
      stdout.flush()
      let input = readLine(stdin)
      try:
        choice = parseInt(input.strip())
        let index = choice - 1
        if index >= 0 and index < board.len() and board[index].isDigit():
          board[index] = currentPlayer
          validMove = true
        else:
          echo "Invalid move. That cell is already taken or out of bounds. Please choose an empty cell from 1 to 9."
      except ValueError:
        echo "Invalid input. Please enter a number from 1 to 9."
      except IndexError:
        echo "Invalid input. Please enter a number from 1 to 9."
    if checkWin(currentPlayer):
      displayBoard()
      echo "Player " & currentPlayer & " wins! Congratulations!"
      gameOver = true
    elif checkDraw():
      displayBoard()
      echo "It's a draw!"
      gameOver = true
    else:
      if currentPlayer == "X":
        currentPlayer = "O"
      else:
        currentPlayer = "X"
  echo "Game over. Thanks for playing!"

when isMainModule:
  initializeBoard()
  playGame()

