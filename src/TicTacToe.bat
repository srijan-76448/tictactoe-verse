@echo off
REM tic_tac_toe.bat

REM Initialize the game board
set cell0=1
set cell1=2
set cell2=3
set cell3=4
set cell4=5
set cell5=6
set cell6=7
set cell7=8
set cell8=9

set current_player=X
set game_over=0

cls

echo Welcome to Tic-Tac-Toe!
echo Players use 'X' and 'O'. Enter the number of the cell (1-9) to place your mark.
echo.
goto game_loop

REM Function to display the board
:display_board
cls
echo.
echo --- Tic-Tac-Toe Board ---
echo -------------
echo ^| %cell0% ^| %cell1% ^| %cell2% ^|
echo -------------
echo ^| %cell3% ^| %cell4% ^| %cell5% ^|
echo -------------
echo ^| %cell6% ^| %cell7% ^| %cell8% ^|
echo -------------
echo -------------------------
echo.
goto :EOF

REM Function to check for a win condition
REM Parameter is current_player_mark (passed via global 'current_player')
REM Sets game_over to 1 if win found.
:check_win
set win=0

REM Check Rows
if "%cell0%"=="%current_player%" if "%cell1%"=="%current_player%" if "%cell2%"=="%current_player%" set win=1
if "%cell3%"=="%current_player%" if "%cell4%"=="%current_player%" if "%cell5%"=="%current_player%" set win=1
if "%cell6%"=="%current_player%" if "%cell7%"=="%current_player%" if "%cell8%"=="%current_player%" set win=1

REM Check Columns
if "%cell0%"=="%current_player%" if "%cell3%"=="%current_player%" if "%cell6%"=="%current_player%" set win=1
if "%cell1%"=="%current_player%" if "%cell4%"=="%current_player%" if "%cell7%"=="%current_player%" set win=1
if "%cell2%"=="%current_player%" if "%cell5%"=="%current_player%" if "%cell8%"=="%current_player%" set win=1

REM Check Diagonals
if "%cell0%"=="%current_player%" if "%cell4%"=="%current_player%" if "%cell8%"=="%current_player%" set win=1
if "%cell2%"=="%current_player%" if "%cell4%"=="%current_player%" if "%cell6%"=="%current_player%" set win=1

if %win% EQU 1 (
    set game_over=1
)
goto :EOF

REM Function to check for a draw condition
REM Sets game_over to 1 if draw found.
:check_draw
set draw=1
REM If any cell is still a number, it's not a draw
for %%c in (0 1 2 3 4 5 6 7 8) do (
    call :get_cell_value %%c cell_value
    if "!cell_value!" GTR "0" if "!cell_value!" LSS "10" set draw=0
)

if %draw% EQU 1 (
    set game_over=1
)
goto :EOF

REM Helper to get cell value (due to delayed expansion issues with %var% in loops)
:get_cell_value
setlocal enabledelayedexpansion
for /f "tokens=*" %%v in ('echo !cell%1!') do (
    endlocal
    set %2=%%v
    goto :EOF
)

REM Main game loop
:game_loop
call :display_board

if %game_over% EQU 1 goto end_game

:get_move
echo Player %current_player%'s turn.
set /p choice="Enter your move (1-9): "

REM Input validation
if not "%choice%" geq "1" goto invalid_move
if not "%choice%" leq "9" goto invalid_move

set index=%choice%-1

REM Check if the cell is empty (contains a number)
call :get_cell_value %index% selected_cell_value
if "%selected_cell_value%" GTR "0" if "%selected_cell_value%" LSS "10" (
    REM Valid move, place mark
    if %index% EQU 0 set cell0=%current_player%
    if %index% EQU 1 set cell1=%current_player%
    if %index% EQU 2 set cell2=%current_player%
    if %index% EQU 3 set cell3=%current_player%
    if %index% EQU 4 set cell4=%current_player%
    if %index% EQU 5 set cell5=%current_player%
    if %index% EQU 6 set cell6=%current_player%
    if %index% EQU 7 set cell7=%current_player%
    if %index% EQU 8 set cell8=%current_player%
    goto check_game_state
) else (
    :invalid_move
    echo Invalid move. That cell is already taken or out of bounds. Please choose an empty cell from 1 to 9.
    echo.
    goto get_move
)

:check_game_state
call :check_win
if %game_over% EQU 1 (
    call :display_board
    echo Player %current_player% wins! Congratulations!
    goto end_game
)

call :check_draw
if %game_over% EQU 1 (
    call :display_board
    echo It's a draw!
    goto end_game
)

REM Switch player
if "%current_player%"=="X" (
    set current_player=O
) else (
    set current_player=X
)
goto game_loop

:end_game
echo.
echo Game over. Thanks for playing!
echo.
pause > NUL
exit /b 0
