HAI

BTW Tic-Tac-Toe Game in LOLCODE

I HAS A BOARD ITZ A LIST OF STRINGS
I HAS A CURRENT_PLAYER ITZ "X"
I HAS A GAME_OVER ITZ NO

HOW DUZ I INIT_BOARD
    BOARD R LYK YAR LIST OF "" "" "" "" "" "" "" "" ""
    I HAS A I ITZ 0
    IM IN YR LOOP_INIT UPPIN YR I TIL I IS GRTR THAN 8
        BOARD @ I WUZ $(SUM OF I AN 1)$
    IM OUTTA YR LOOP_INIT
MKAY

HOW DUZ I DISPLAY_BOARD
    VISIBLE ""
    VISIBLE "--- Tic-Tac-Toe Board ---"
    VISIBLE "-------------"
    I HAS A I ITZ 0
    IM IN YR LOOP_DISPLAY UPPIN YR I TIL I IS GRTR THAN 6 STEP 3
        VISIBLE "| " + BOARD @ I + " | " + BOARD @ (SUM OF I AN 1) + " | " + BOARD @ (SUM OF I AN 2) + " |"
        VISIBLE "-------------"
    IM OUTTA YR LOOP_DISPLAY
    VISIBLE "-------------------------"
    VISIBLE ""
MKAY

HOW DUZ I CHECK_WIN_SIMPLIFIED YR PLAYER_MARK
    I HAS A FOUND_WIN ITZ NO
    O RLY? BOARD @ 0 IS PLAYER_MARK AN BOARD @ 1 IS PLAYER_MARK AN BOARD @ 2 IS PLAYER_MARK
        YA RLY
            FOUND_WIN ITZ AN_BIG_TRUE
    OIC
    GTFO WITH FOUND_WIN
MKAY

HOW DUZ I CHECK_DRAW_SIMPLIFIED
    I HAS A FILLED_COUNT ITZ 0
    I HAS A I ITZ 0
    IM IN YR LOOP_DRAW UPPIN YR I TIL I IS GRTR THAN 8
        I HAS A CELL_CONTENT ITZ BOARD @ I
        O RLY? CELL_CONTENT IS "X" AN CELL_CONTENT IS "O"
            NO WAI
                BTW If it's a number (not 'X' or 'O'), it's not filled
                FILLED_COUNT ITZ SUM OF FILLED_COUNT AN 1
        OIC
    IM OUTTA YR LOOP_DRAW

    O RLY? FILLED_COUNT IS 9
        YA RLY
            IS_DRAW ITZ AN_BIG_TRUE
        NO WAI
            IS_DRAW ITZ AN_BIG_FALSE
    OIC
    GTFO WITH IS_DRAW
MKAY

IM IN YR GAME_LOOP WILE NOT GAME_OVER
    DISPLAY_BOARD

    I HAS A VALID_MOVE ITZ NO
    I HAS A CHOICE_NUM ITZ 0

    IM IN YR MOVE_LOOP WILE NOT VALID_MOVE
        VISIBLE "Player " + CURRENT_PLAYER + "'s turn. Enter your move (1-9): "
        GIMMEH CHOICE_STR
        
        BTW Very basic string-to-number conversion.
        BTW This is highly simplified and only handles single digits '1'-'9'.
        O RLY? CHOICE_STR IS "1"
            YA RLY CHOICE_NUM ITZ 1
            NO WAI O RLY? CHOICE_STR IS "2"
                YA RLY CHOICE_NUM ITZ 2
                NO WAI O RLY? CHOICE_STR IS "3"
                    YA RLY CHOICE_NUM ITZ 3
                    NO WAI O RLY? CHOICE_STR IS "4"
                        YA RLY CHOICE_NUM ITZ 4
                        NO WAI O RLY? CHOICE_STR IS "5"
                            YA RLY CHOICE_NUM ITZ 5
                            NO WAI O RLY? CHOICE_STR IS "6"
                                YA RLY CHOICE_NUM ITZ 6
                                NO WAI O RLY? CHOICE_STR IS "7"
                                    YA RLY CHOICE_NUM ITZ 7
                                    NO WAI O RLY? CHOICE_STR IS "8"
                                        YA RLY CHOICE_NUM ITZ 8
                                        NO WAI O RLY? CHOICE_STR IS "9"
                                            YA RLY CHOICE_NUM ITZ 9
                                            NO WAI
                                                VISIBLE "Invalid input. Please enter a number from 1 to 9."
                                                GTFO FROM MOVE_LOOP BTW break from this loop if not a valid digit
                                        OIC
                                    OIC
                                OIC
                            OIC
                        OIC
                    OIC
                OIC
            OIC

        BTW Check if a valid number was actually parsed (0 means it failed to match '1'-'9')
        O RLY? CHOICE_NUM IS GRTR THAN 0
            YA RLY
                I HAS A INDEX ITZ DIFF OF CHOICE_NUM AN 1
                I HAS A CELL_VAL ITZ BOARD @ INDEX

                BTW Check if the cell is empty (contains a number, not 'X' or 'O')
                O RLY? CELL_VAL NOT EQUALZ "X" AN CELL_VAL NOT EQUALZ "O"
                    YA RLY
                        BOARD @ INDEX WUZ CURRENT_PLAYER
                        VALID_MOVE ITZ AN_BIG_TRUE
                    NO WAI
                        VISIBLE "Invalid move. That cell is already taken. Please choose an empty cell."
                OIC
            NO WAI
                BTW This branch is for general invalid non-numeric input already handled above
        OIC
    IM OUTTA YR MOVE_LOOP

    BTW Check for win (simplified)
    I HAS A PLAYER_WON ITZ CHECK_WIN_SIMPLIFIED YR CURRENT_PLAYER
    O RLY? PLAYER_WON
        YA RLY
            DISPLAY_BOARD
            VISIBLE "Player " + CURRENT_PLAYER + " wins! Congratulations!"
            GAME_OVER ITZ AN_BIG_TRUE
        NO WAI
            BTW Check for draw (simplified)
            I HAS A GAME_IS_DRAW ITZ CHECK_DRAW_SIMPLIFIED
            O RLY? GAME_IS_DRAW
                YA RLY
                    DISPLAY_BOARD
                    VISIBLE "It's a draw!"
                    GAME_OVER ITZ AN_BIG_TRUE
                NO WAI
                    BTW If no win or draw, switch player
                    O RLY? CURRENT_PLAYER IS "X"
                        YA RLY CURRENT_PLAYER ITZ "O"
                        NO WAI CURRENT_PLAYER ITZ "X"
                    OIC
            OIC
    OIC
IM OUTTA YR GAME_LOOP

VISIBLE "Game over. Thanks for playing!"
