defmodule TicTacToe do
  @moduledoc """
  A simple command-line Tic-Tac-Toe game implemented in Elixir.
  Players take turns entering a number from 1 to 9 to place their mark.
  """

  def initialize_board() do
    1..9 |> Enum.to_list()
  end

  def display_board(board) do
    IO.puts "\n--- Tic-Tac-Toe Board ---"
    IO.puts "-------------"
    [r1, r2, r3] = Enum.chunk_every(board, 3)

    IO.puts ~s(| #{format_cell(r1 |> Enum.at(0))} | #{format_cell(r1 |> Enum.at(1))} | #{format_cell(r1 |> Enum.at(2))} |)
    IO.puts "-------------"
    IO.puts ~s(| #{format_cell(r2 |> Enum.at(0))} | #{format_cell(r2 |> Enum.at(1))} | #{format_cell(r2 |> Enum.at(2))} |)
    IO.puts "-------------"
    IO.puts ~s(| #{format_cell(r3 |> Enum.at(0))} | #{format_cell(r3 |> Enum.at(1))} | #{format_cell(r3 |> Enum.at(2))} |)
    IO.puts "-------------"
    IO.puts "-------------------------\n"
  end

  defp format_cell(val) when is_atom(val), do: String.upcase(Atom.to_string(val))
  defp format_cell(val), do: to_string(val)

  def check_win(board, player) do
    win_conditions = [
      [0, 1, 2], [3, 4, 5], [6, 7, 8], # Rows
      [0, 3, 6], [1, 4, 7], [2, 5, 8], # Columns
      [0, 4, 8], [2, 4, 6]            # Diagonals
    ]

    Enum.any?(win_conditions, fn [i1, i2, i3] ->
      (Enum.at(board, i1) == player) and
      (Enum.at(board, i2) == player) and
      (Enum.at(board, i3) == player)
    end)
  end

  def check_draw(board) do
    Enum.all?(board, fn cell -> not is_integer(cell) end)
  end

  def get_player_choice(current_player) do
    IO.puts ~s(Player #{String.upcase(Atom.to_string(current_player))}'s turn.)
    IO.write "Enter your move (1-9): "
    input = IO.read(:line) |> String.trim()

    case Integer.parse(input) do
      {choice, ""} when choice >= 1 and choice <= 9 -> choice
      _ -> 0
    end
  end

  def make_move(old_board, player, choice) when is_integer(choice) and choice >= 1 and choice <= 9 do
    index = choice - 1
    case Enum.at(old_board, index) do
      cell when is_integer(cell) ->
        new_board = Enum.replace_at(old_board, index, player)
        {:ok, new_board}
      _ ->
        {:error, :taken}
    end
  end
  def make_move(_old_board, _player, _choice), do: {:error, :invalid_input}

  def play_game(board, current_player) do
    display_board(board)

    cond do
      check_win(board, current_player) ->
        IO.puts ~s(Player #{String.upcase(Atom.to_string(current_player))} wins! Congratulations!)
        IO.puts "Game over. Thanks for playing!"
      check_draw(board) ->
        IO.puts "It's a draw!"
        IO.puts "Game over. Thanks for playing!"
      true ->
        choice = get_player_choice(current_player)
        case make_move(board, current_player, choice) do
          {:ok, new_board} ->
            next_player = if current_player == :x, do: :o, else: :x
            play_game(new_board, next_player)
          {:error, :taken} ->
            IO.puts "Invalid move. That cell is already taken. Please choose an empty cell."
            play_game(board, current_player)
          {:error, :invalid_input} ->
            IO.puts "Invalid input. Please enter a number from 1-9 for an empty cell."
            play_game(board, current_player)
        end
    end
  end

  def start() do
    IO.puts "Welcome to Tic-Tac-Toe!"
    IO.puts "Players use 'X' and 'O'. Enter the number of the cell (1-9) to place your mark."
    initial_board = initialize_board()
    play_game(initial_board, :x)
  end
end
