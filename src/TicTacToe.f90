! tic_tac_toe.f90

PROGRAM tic_tac_toe

  CHARACTER(LEN=1) :: board(3,3)     ! The 3x3 game board ('1'-'9', 'X', 'O')
  CHARACTER(LEN=1) :: current_player ! 'X' or 'O'
  LOGICAL          :: game_over      ! .TRUE. if game is finished, .FALSE. otherwise
  INTEGER          :: choice         ! Player's chosen cell number (1-9)
  INTEGER          :: row, col       ! Row and column derived from choice
  LOGICAL          :: valid_move     ! .TRUE. if move is valid, .FALSE. otherwise

  INTERFACE
    SUBROUTINE initialize_board(board_in)
      CHARACTER(LEN=1), INTENT(OUT) :: board_in(3,3)
    END SUBROUTINE initialize_board

    SUBROUTINE display_board(board_in)
      CHARACTER(LEN=1), INTENT(IN) :: board_in(3,3)
    END SUBROUTINE display_board

    FUNCTION check_win(board_in, player_mark) RESULT(is_won)
      CHARACTER(LEN=1), INTENT(IN) :: board_in(3,3)
      CHARACTER(LEN=1), INTENT(IN) :: player_mark
      LOGICAL :: is_won
    END FUNCTION check_win

    FUNCTION check_draw(board_in) RESULT(is_draw)
      CHARACTER(LEN=1), INTENT(IN) :: board_in(3,3)
      LOGICAL :: is_draw
    END FUNCTION check_draw
  END INTERFACE

  CALL initialize_board(board) ! Setup the initial game board
  current_player = 'X'        ! 'X' starts the game
  game_over = .FALSE.         ! Game is not over yet

  PRINT *, "Welcome to Tic-Tac-Toe!"
  PRINT *, "Players use 'X' and 'O'. Enter the number of the cell (1-9) to place your mark."

  DO WHILE (.NOT. game_over)
    CALL display_board(board) ! Show the current state of the board

    valid_move = .FALSE. ! Reset valid move flag for each turn

    DO WHILE (.NOT. valid_move)
      PRINT *, "Player ", current_player, "'s turn. Enter your move (1-9): "
      READ (*,*, IOSTAT=choice_status) choice ! Read player's input

      IF (choice_status /= 0) THEN ! Check for read errors (e.g., non-numeric input)
        PRINT *, "Invalid input. Please enter a number."
        CYCLE ! Go to next iteration of inner loop to re-prompt
      END IF

      row = (choice - 1) / 3 + 1
      col = (choice - 1) MOD 3 + 1

      IF (choice >= 1 .AND. choice <= 9 .AND. &
          board(row,col) >= '1' .AND. board(row,col) <= '9') THEN
        board(row,col) = current_player ! Place the player's mark
        valid_move = .TRUE.            ! Mark move as valid
      ELSE IF (choice < 1 .OR. choice > 9) THEN
        PRINT *, "Invalid choice. Please enter a number from 1 to 9."
      ELSE
        PRINT *, "Invalid move. That cell is already taken. Please choose an empty cell."
      END IF
    END DO

    IF (check_win(board, current_player)) THEN
      CALL display_board(board) ! Show the final board with the winning mark
      PRINT *, "Player ", current_player, " wins! Congratulations!"
      game_over = .TRUE.       ! Game is over
    ELSE IF (check_draw(board)) THEN
      CALL display_board(board) ! Show the final board for a draw
      PRINT *, "It's a draw!"
      game_over = .TRUE.       ! Game is over
    ELSE
      IF (current_player == 'X') THEN
        current_player = 'O'
      ELSE
        current_player = 'X'
      END IF
    END IF
  END DO

  PRINT *, "Game over. Thanks for playing!"

CONTAINS

  SUBROUTINE initialize_board(board_in)
    CHARACTER(LEN=1), INTENT(OUT) :: board_in(3,3) ! Board passed by reference

    INTEGER :: i, j, cell_num
    cell_num = 1
    DO i = 1, 3
      DO j = 1, 3
        board_in(i,j) = CHAR(ICHAR('0') + cell_num) ! Convert number to character ('1'-'9')
        cell_num = cell_num + 1
      END DO
    END DO
  END SUBROUTINE initialize_board

  SUBROUTINE display_board(board_in)
    CHARACTER(LEN=1), INTENT(IN) :: board_in(3,3) ! Board passed by reference (read-only)

    INTEGER :: i
    PRINT *, "--- Tic-Tac-Toe Board ---"
    PRINT *, "-------------"
    DO i = 1, 3
      PRINT "(A2,A1,A3,A1,A3,A1,A2)", "| ", board_in(i,1), " | ", board_in(i,2), " | ", board_in(i,3), " |"
      PRINT *, "-------------"
    END DO
    PRINT *, "-------------------------\n"
  END SUBROUTINE display_board

  FUNCTION check_win(board_in, player_mark) RESULT(is_won)
    CHARACTER(LEN=1), INTENT(IN) :: board_in(3,3)
    CHARACTER(LEN=1), INTENT(IN) :: player_mark
    LOGICAL :: is_won

    is_won = .FALSE. ! Assume no win initially

    DO row = 1, 3
      IF (board_in(row,1) == player_mark .AND. &
          board_in(row,2) == player_mark .AND. &
          board_in(row,3) == player_mark) THEN
        is_won = .TRUE.
        RETURN ! Exit function early if win found
      END IF
    END DO

    DO col = 1, 3
      IF (board_in(1,col) == player_mark .AND. &
          board_in(2,col) == player_mark .AND. &
          board_in(3,col) == player_mark) THEN
        is_won = .TRUE.
        RETURN ! Exit function early if win found
      END IF
    END DO

    IF (board_in(1,1) == player_mark .AND. &
        board_in(2,2) == player_mark .AND. &
        board_in(3,3) == player_mark) THEN
      is_won = .TRUE.
      RETURN
    END IF

    IF (board_in(1,3) == player_mark .AND. &
        board_in(2,2) == player_mark .AND. &
        board_in(3,1) == player_mark) THEN
      is_won = .TRUE.
      RETURN
    END IF

    ! If execution reaches here, no win was found, so is_won remains .FALSE.
  END FUNCTION check_win

  FUNCTION check_draw(board_in) RESULT(is_draw)
    CHARACTER(LEN=1), INTENT(IN) :: board_in(3,3)
    LOGICAL :: is_draw
    INTEGER :: i, j

    is_draw = .TRUE. ! Assume draw initially

    DO i = 1, 3
      DO j = 1, 3
        IF (board_in(i,j) >= '1' .AND. board_in(i,j) <= '9') THEN
          is_draw = .FALSE. ! Found an empty cell, not a draw yet
          RETURN ! Exit function early
        END IF
      END DO
    END DO
    ! If execution reaches here, no empty cells were found, so is_draw remains .TRUE.
  END FUNCTION check_draw

END PROGRAM tic_tac_toe
