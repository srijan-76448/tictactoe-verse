; File: tic_tac_toe.asm
; Assembly Language: x86-64 (Linux)
; Assembler: NASM
; Linker: LD
;
; Description: A simple command-line Tic-Tac-Toe game written in NASM x86-64 assembly.
;              Players take turns entering a number from 1 to 9 to place their mark.
;              The board is represented as a 1D array.
;
; To compile and run:
;   nasm -f elf64 tic_tac_toe.asm -o tic_tac_toe.o
;   ld tic_tac_toe.o -o tic_tac_toe
;   ./tic_tac_toe

section .data
    board db '         '
    player_x db 'X'
    player_o db 'O'
    msg_welcome db "Welcome to Tic-Tac-Toe!", 10
    len_welcome equ $-msg_welcome
    msg_instructions db "Enter a number 1-9 to place your mark (1 is top-left, 9 is bottom-right).", 10
    len_instructions equ $-msg_instructions
    msg_current_player db "Player ", 0
    len_current_player equ $-msg_current_player
    msg_turn_suffix db "'s turn. Enter your move: ", 0
    len_turn_suffix equ $-msg_turn_suffix
    msg_invalid_move db "Invalid move! Please enter a number from 1-9 for an empty cell.", 10
    len_invalid_move equ $-msg_invalid_move
    msg_player_wins db "Player ", 0
    len_player_wins equ $-msg_player_wins
    msg_win_suffix db " wins! Congratulations!", 10
    len_win_suffix equ $-msg_win_suffix
    msg_draw db "It's a draw!", 10
    len_draw equ $-msg_draw
    msg_game_over db "Game over. Thanks for playing!", 10
    len_game_over equ $-msg_game_over
    horizontal_line db "-----------", 10
    len_horizontal_line equ $-horizontal_line
    vertical_sep db " | ", 0
    len_vertical_sep equ $-vertical_sep
    newline db 10
    len_newline equ $-newline

section .bss
    input_char resb 1

section .text
    global _start

%macro SYS_WRITE 2
    mov rax, 1
    mov rdi, %1
    mov rsi, %2
    syscall
%endmacro

%macro SYS_READ 2
    mov rax, 0
    mov rdi, %1
    mov rsi, %2
    mov rdx, 1
    syscall
%endmacro

_start:
    call display_welcome
    call game_loop
    call exit_program

display_welcome:
    mov rdx, len_welcome
    SYS_WRITE 1, msg_welcome
    mov rdx, len_instructions
    SYS_WRITE 1, msg_instructions
    ret

display_board:
    push rbp
    mov rbp, rsp
    push rsi
    push rcx
    push rbx
    mov rdx, len_horizontal_line
    SYS_WRITE 1, horizontal_line
    mov rsi, board
    mov rcx, 0
.loop_board:
    mov rdx, 1
    SYS_WRITE 1, rsi
    inc rcx
    inc rsi
    cmp rcx, 1
    je .print_separator
    cmp rcx, 2
    je .print_separator
    jmp .check_row_end
.print_separator:
    mov rdx, len_vertical_sep
    SYS_WRITE 1, vertical_sep
.check_row_end:
    cmp rcx, 3
    je .end_row
    cmp rcx, 6
    je .end_row
    cmp rcx, 9
    je .end_row
    jmp .continue_loop
.end_row:
    mov rdx, len_newline
    SYS_WRITE 1, newline
    mov rdx, len_horizontal_line
    SYS_WRITE 1, horizontal_line
.continue_loop:
    cmp rcx, 9
    jb .loop_board
    pop rbx
    pop rcx
    pop rsi
    pop rbp
    ret

get_move:
    push rbp
    mov rbp, rsp
    push rbx
    push rcx
    push rdx
    push rsi
    mov rdx, len_current_player
    SYS_WRITE 1, msg_current_player
    mov rdx, 1
    SYS_WRITE 1, byte [rbx]
    mov rdx, len_turn_suffix
    SYS_WRITE 1, msg_turn_suffix
.read_input_loop:
    SYS_READ 0, input_char
    mov al, byte [input_char]
    sub al, '0'
    cmp al, 1
    jl .invalid_input_prompt
    cmp al, 9
    jg .invalid_input_prompt
    movzx rbx, al
    dec rbx
    mov sil, byte [board + rbx]
    cmp sil, ' '
    je .valid_move
.invalid_input_prompt:
    mov rdx, len_invalid_move
    SYS_WRITE 1, msg_invalid_move
    mov rdx, len_current_player
    SYS_WRITE 1, msg_current_player
    mov rdx, 1
    SYS_WRITE 1, byte [rbx_current_player_marker]
    mov rdx, len_turn_suffix
    SYS_WRITE 1, msg_turn_suffix
    jmp .read_input_loop
.valid_move:
    mov rax, rbx
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rbp
    ret

check_win:
    push rbp
    mov rbp, rsp
    push rbx
    push rcx
    push rdx
    push rsi
    mov rax, 0
    mov cl, byte [board + 0]
    cmp cl, bl
    jne .check_row2
    mov cl, byte [board + 1]
    cmp cl, bl
    jne .check_row2
    mov cl, byte [board + 2]
    cmp cl, bl
    jne .check_row2
    mov rax, 1
    jmp .end_check_win
.check_row2:
    mov cl, byte [board + 3]
    cmp cl, bl
    jne .check_row3
    mov cl, byte [board + 4]
    cmp cl, bl
    jne .check_row3
    mov cl, byte [board + 5]
    cmp cl, bl
    jne .check_row3
    mov rax, 1
    jmp .end_check_win
.check_row3:
    mov cl, byte [board + 6]
    cmp cl, bl
    jne .check_cols
    mov cl, byte [board + 7]
    cmp cl, bl
    jne .check_cols
    mov cl, byte [board + 8]
    cmp cl, bl
    jne .check_cols
    mov rax, 1
    jmp .end_check_win
.check_cols:
    mov cl, byte [board + 0]
    cmp cl, bl
    jne .check_col2
    mov cl, byte [board + 3]
    cmp cl, bl
    jne .check_col2
    mov cl, byte [board + 6]
    cmp cl, bl
    jne .check_col2
    mov rax, 1
    jmp .end_check_win
.check_col2:
    mov cl, byte [board + 1]
    cmp cl, bl
    jne .check_col3
    mov cl, byte [board + 4]
    cmp cl, bl
    jne .check_col3
    mov cl, byte [board + 7]
    cmp cl, bl
    jne .check_col3
    mov rax, 1
    jmp .end_check_win
.check_col3:
    mov cl, byte [board + 2]
    cmp cl, bl
    jne .check_diagonals
    mov cl, byte [board + 5]
    cmp cl, bl
    jne .check_diagonals
    mov cl, byte [board + 8]
    cmp cl, bl
    jne .check_diagonals
    mov rax, 1
    jmp .end_check_win
.check_diagonals:
    mov cl, byte [board + 0]
    cmp cl, bl
    jne .check_diag2
    mov cl, byte [board + 4]
    cmp cl, bl
    jne .check_diag2
    mov cl, byte [board + 8]
    cmp cl, bl
    jne .check_diag2
    mov rax, 1
    jmp .end_check_win
.check_diag2:
    mov cl, byte [board + 2]
    cmp cl, bl
    jne .end_check_win
    mov cl, byte [board + 4]
    cmp cl, bl
    jne .end_check_win
    mov cl, byte [board + 6]
    cmp cl, bl
    jne .end_check_win
    mov rax, 1
.end_check_win:
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rbp
    ret

check_draw:
    push rbp
    mov rbp, rsp
    push rcx
    push rsi
    mov rax, 1
    mov rcx, 0
    mov rsi, board
.loop_cells:
    mov cl, byte [rsi + rcx]
    cmp cl, ' '
    je .not_draw
    inc rcx
    cmp rcx, 9
    jb .loop_cells
    jmp .end_check_draw
.not_draw:
    mov rax, 0
.end_check_draw:
    pop rsi
    pop rcx
    pop rbp
    ret

game_loop:
    push rbp
    mov rbp, rsp
    push r12
    push r13
    push r14
    mov r12, 0
    mov r13b, byte [player_x]
.loop_game:
    cmp r12, 1
    je .end_game_loop
    call display_board
    mov rbx_current_player_marker, r13b
    call get_move
    mov r14, rax
    mov byte [board + r14], r13b
    mov bl, r13b
    call check_win
    cmp rax, 1
    je .player_won
    call check_draw
    cmp rax, 1
    je .game_draw
    cmp r13b, byte [player_x]
    je .switch_to_o
    jmp .switch_to_x
.switch_to_o:
    mov r13b, byte [player_o]
    jmp .loop_game
.switch_to_x:
    mov r13b, byte [player_x]
    jmp .loop_game
.player_won:
    call display_board
    mov rdx, len_player_wins
    SYS_WRITE 1, msg_player_wins
    mov rdx, 1
    SYS_WRITE 1, r13b
    mov rdx, len_win_suffix
    SYS_WRITE 1, msg_win_suffix
    mov r12, 1
    jmp .loop_game
.game_draw:
    call display_board
    mov rdx, len_draw
    SYS_WRITE 1, msg_draw
    mov r12, 1
    jmp .loop_game
.end_game_loop:
    pop r14
    pop r13
    pop r12
    pop rbp
    ret

exit_program:
    mov rdx, len_game_over
    SYS_WRITE 1, msg_game_over
    mov rax, 60
    xor rdi, rdi
    syscall
