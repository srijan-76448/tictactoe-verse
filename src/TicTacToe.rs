// main.rs

use std::io;

fn initialize_board() -> Vec<char> {
    let mut board = Vec::with_capacity(9);
    for i in 0..9 {
        board.push(std::char::from_digit((i + 1) as u32, 10).unwrap());
    }
    board
}

fn display_board(board: &Vec<char>) {
    println!("\n--- Tic-Tac-Toe Board ---");
    println!("-------------");
    for i in 0..3 {
        let row_start_index = i * 3;
        println!(
            "| {} | {} | {} |",
            board[row_start_index],
            board[row_start_index + 1],
            board[row_start_index + 2]
        );
        println!("-------------");
    }
    println!("-------------------------\n");
}

fn check_win(board: &Vec<char>, player_mark: char) -> bool {
    let win_conditions: [[usize; 3]; 8] = [
        [0, 1, 2],
        [3, 4, 5],
        [6, 7, 8],
        [0, 3, 6],
        [1, 4, 7],
        [2, 5, 8],
        [0, 4, 8],
        [2, 4, 6],
    ];

    for condition in win_conditions.iter() {
        if board[condition[0]] == player_mark
            && board[condition[1]] == player_mark
            && board[condition[2]] == player_mark
        {
            return true;
        }
    }
    false
}

fn check_draw(board: &Vec<char>) -> bool {
    board.iter().all(|&cell| !cell.is_digit(10))
}

fn main() {
    let mut board = initialize_board();
    let mut current_player = 'X';
    let mut game_over = false;

    println!("Welcome to Tic-Tac-Toe!");
    println!("Players use 'X' and 'O'. Enter the number of the cell (1-9) to place your mark.");

    while !game_over {
        display_board(&board);

        let mut valid_move = false;
        let mut choice_str = String::new();

        while !valid_move {
            print!("Player {}'s turn. Enter your move (1-9): ", current_player);
            io::stdout().flush().expect("Failed to flush stdout");

            choice_str.clear();
            io::stdin()
                .read_line(&mut choice_str)
                .expect("Failed to read line");

            let choice: Result<usize, _> = choice_str.trim().parse();

            match choice {
                Ok(num) => {
                    let index = num - 1;

                    if index < board.len() && board[index].is_digit(10) {
                        board[index] = current_player;
                        valid_move = true;
                    } else if index >= board.len() {
                        println!("Invalid input. Please enter a number from 1 to 9.");
                    } else {
                        println!("Invalid move. That cell is already taken. Please choose an empty cell.");
                    }
                }
                Err(_) => {
                    println!("Invalid input. Please enter a number from 1 to 9.");
                }
            }
        }

        if check_win(&board, current_player) {
            display_board(&board);
            println!("Player {} wins! Congratulations!", current_player);
            game_over = true;
        } else if check_draw(&board) {
            display_board(&board);
            println!("It's a draw!");
            game_over = true;
        } else {
            current_player = if current_player == 'X' { 'O' } else { 'X' };
        }
    }

    println!("Game over. Thanks for playing!");
}

