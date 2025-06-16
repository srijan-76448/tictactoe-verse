def initialize_board
  (1..9).to_a
end

def display_board(board)
  puts "\n--- Tic-Tac-Toe Board ---"
  puts "-------------"
  (0..2).each do |i|
    row_start_index = i * 3
    puts "| #{board[row_start_index]} | #{board[row_start_index + 1]} | #{board[row_start_index + 2]} |"
    puts "-------------"
  end
  puts "-------------------------\n"
end

def check_win(board, player_mark)
  win_conditions = [
    [0, 1, 2], [3, 4, 5], [6, 7, 8],
    [0, 3, 6], [1, 4, 7], [2, 5, 8],
    [0, 4, 8], [2, 4, 6]
  ]

  win_conditions.any? do |condition|
    board[condition[0]] == player_mark &&
      board[condition[1]] == player_mark &&
      board[condition[2]] == player_mark
  end
end

def check_draw(board)
  board.all? { |cell| cell == 'X' || cell == 'O' }
end

def play_game
  board = initialize_board
  current_player = 'X'
  game_over = false

  puts "Welcome to Tic-Tac-Toe!"
  puts "Players use 'X' and 'O'. Enter the number of the cell (1-9) to place your mark."

  until game_over
    display_board(board)

    valid_move = false
    choice = nil

    until valid_move
      print "Player #{current_player}'s turn. Enter your move (1-9): "
      input = gets.chomp

      if input =~ /^[1-9]$/
        choice = input.to_i
        index = choice - 1

        if board[index].is_a?(Integer)
          board[index] = current_player
          valid_move = true
        else
          puts "Invalid move. That cell is already taken. Please choose an empty cell."
        end
      else
        puts "Invalid input. Please enter a number from 1 to 9."
      end
    end

    if check_win(board, current_player)
      display_board(board)
      puts "Player #{current_player} wins! Congratulations!"
      game_over = true
    elsif check_draw(board)
      display_board(board)
      puts "It's a draw!"
      game_over = true
    else
      current_player = (current_player == 'X') ? 'O' : 'X'
    end
  end

  puts "Game over. Thanks for playing!"
end

play_game
