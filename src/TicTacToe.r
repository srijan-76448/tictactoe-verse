# tic_tac_toe.R

initialize_board <- function() {
  board <- as.character(1:9)
  return(board)
}

display_board <- function(board) {
  cat("\n--- Tic-Tac-Toe Board ---\n")
  cat("-------------\n")
  for (i in 0:2) {
    row_start_index <- i * 3
    cat(sprintf("| %s | %s | %s |\n",
                board[row_start_index + 1],
                board[row_start_index + 2],
                board[row_start_index + 3]))
    cat("-------------\n")
  }
  cat("-------------------------\n\n")
}

check_win <- function(board, player_mark) {
  win_conditions <- list(
    c(1, 2, 3), c(4, 5, 6), c(7, 8, 9),
    c(1, 4, 7), c(2, 5, 8), c(3, 6, 9),
    c(1, 5, 9), c(3, 5, 7)
  )

  for (condition in win_conditions) {
    if (board[condition[1]] == player_mark &&
        board[condition[2]] == player_mark &&
        board[condition[3]] == player_mark) {
      return(TRUE)
    }
  }
  return(FALSE)
}

check_draw <- function(board) {
  if (any(!is.na(suppressWarnings(as.numeric(board))))) {
    return(FALSE)
  }
  return(TRUE)
}

play_game <- function() {
  board <- initialize_board()
  current_player <- 'X'
  game_over <- FALSE

  cat("Welcome to Tic-Tac-Toe!\n")
  cat("Players use 'X' and 'O'. Enter the number of the cell (1-9) to place your mark.\n")

  while (!game_over) {
    display_board(board)

    valid_move <- FALSE
    choice <- NULL

    while (!valid_move) {
      cat(sprintf("Player %s's turn. Enter your move (1-9): ", current_player))

      input <- readLines(con="stdin", n=1)
      num_input <- suppressWarnings(as.numeric(input))

      if (!is.na(num_input) && num_input >= 1 && num_input <= 9) {
        choice <- as.integer(num_input)

        if (!is.na(suppressWarnings(as.numeric(board[choice])))) {
          board[choice] <<- current_player
          valid_move <- TRUE
        } else {
          cat("Invalid move. That cell is already taken. Please choose an empty cell.\n")
        }
      } else {
        cat("Invalid input. Please enter a number from 1 to 9.\n")
      }
    }

    if (check_win(board, current_player)) {
      display_board(board)
      cat(sprintf("Player %s wins! Congratulations!\n", current_player))
      game_over <- TRUE
    } else if (check_draw(board)) {
      display_board(board)
      cat("It's a draw!\n")
      game_over <- TRUE
    } else {
      current_player <- ifelse(current_player == 'X', 'O', 'X')
    }
  }

  cat("Game over. Thanks for playing!\n")
}

play_game()
