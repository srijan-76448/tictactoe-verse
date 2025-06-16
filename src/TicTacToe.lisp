;; tic-tac-toe.lisp
;; Simple Tic-Tac-Toe Game in Common Lisp

(defvar *board* (make-array 9 :initial-element nil))

(defun initialize-board ()
  (loop for i from 0 to 8
        do (setf (aref *board* i) (+ i 1))))

(defun display-board ()
  (format t "~%--- Tic-Tac-Toe Board ---~%")
  (format t "-------------~%")
  (loop for i from 0 to 2
        do (let ((idx (* i 3)))
             (format t "| ~A | ~A | ~A |~%"
                     (aref *board* idx)
                     (aref *board* (+ idx 1))
                     (aref *board* (+ idx 2)))
             (format t "-------------~%")))
  (format t "-------------------------~%~%"))

(defun check-win (player-mark)
  (let ((win-conditions
          '((0 1 2) (3 4 5) (6 7 8) ; Rows
            (0 3 6) (1 4 7) (2 5 8) ; Columns
            (0 4 8) (2 4 6))))     ; Diagonals
    (loop for condition in win-conditions
          do (let ((c1 (first condition))
                   (c2 (second condition))
                   (c3 (third condition)))
               (when (and (eq (aref *board* c1) player-mark)
                          (eq (aref *board* c2) player-mark)
                          (eq (aref *board* c3) player-mark))
                 (return-from check-win t))))
    nil))

(defun check-draw ()
  (loop for i from 0 to 8
        do (when (integerp (aref *board* i))
             (return-from check-draw nil)))
  t)

(defun get-player-move (current-player)
  (loop
    (format t "Player ~A's turn. Enter your move (1-9): " current-player)
    (let* ((input (read-line))
           (choice (parse-integer input :junk-allowed t))) ; Try to parse integer, ignore extra chars

      (if (and (integerp choice)
               (>= choice 1)
               (<= choice 9))
          (let ((index (- choice 1))) ; Convert 1-9 choice to 0-8 array index
            (if (integerp (aref *board* index)) ; Check if the cell is empty (still a number)
                (return-from get-player-move index)
                (format t "Invalid move. That cell is already taken. Please choose an empty cell.~%")))
          (format t "Invalid input. Please enter a number from 1 to 9.~%")))))

(defun play-game ()
  (initialize-board)       ; Setup the initial game board
  (defvar *current-player* 'X) ; 'X' starts the game (using defvar for initial setting)
  (defvar *game-over* nil)   ; Flag to control the main game loop

  (format t "Welcome to Tic-Tac-Toe!~%")
  (format t "Players use 'X' and 'O'. Enter the number of the cell (1-9) to place your mark.~%")

  (loop while (not *game-over*)
        do (display-board)

           (let ((index (get-player-move *current-player*)))
             (setf (aref *board* index) *current-player*)

             (when (check-win *current-player*)
               (display-board)
               (format t "Player ~A wins! Congratulations!~%" *current-player*)
               (setf *game-over* t))
             (when (check-draw)
               (display-board)
               (format t "It's a draw!~%")
               (setf *game-over* t))
             
             (unless *game-over*
               (setf *current-player* (if (eq *current-player* 'X) 'O' 'X'))))))

  (format t "Game over. Thanks for playing!~%")
  (values)) ; Return NIL to avoid printing the last evaluated expression
