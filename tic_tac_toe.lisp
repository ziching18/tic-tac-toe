;;; TIC-TAC-TOE

;; initialise players
(defun player1()
    (format t "~%Player 1, please enter your name: ")
    (setq name1(read-line))
    (format t "~%~A, you are X. ~%" (string-upcase name1))
)

(defun player2()
    (format t "~%Player 2, please enter your name: ")
    (setq name2(read-line))
    (format t "~%~A, you are O. ~%" (string-upcase name2))
)

;; create and initialise board
(defun make_board()
    (setq Board (make-array 9
        :initial-contents
            '(1 2 3 4 5 6 7 8 9)))
)

(defun cell_value(v)
    (aref Board (- v 1))
)

(defun display_board()
    (format t "~%")
    (format t "     |     |     ~%")
    (format t "   ~A |   ~A |   ~A  ~%" (cell_value 1) (cell_value 2) (cell_value 3))
    (format t "     |     |     ~%")
    (format t "-----------------~%")
    (format t "     |     |     ~%")
    (format t "   ~A |   ~A |   ~A  ~%" (cell_value 4) (cell_value 5) (cell_value 6))
    (format t "     |     |     ~%")
    (format t "-----------------~%")
    (format t "     |     |     ~%")
    (format t "   ~A |   ~A |   ~A  ~%" (cell_value 7) (cell_value 8) (cell_value 9))
    (format t "     |     |     ~%~%")
)

;; use is_line() to check whether player has won by achieving a line
(defun win_game()
    (or (is_line 1 2 3)
        (is_line 1 4 7)
        (is_line 1 5 9)
        (is_line 2 5 8)
        (is_line 3 6 9)
        (is_line 3 5 7)
        (is_line 4 5 6)
        (is_line 7 8 9)) ; only 6 instances
)

(defun is_line (a b c)
    (and
        (equal
            (cell_value a)
            (cell_value b))
        (equal
            (cell_value a)
            (cell_value c)))
)

(defun draw_game()
    (and
        (not (win_game))
        (not (numberp (cell_value 1)))
		(not (numberp (cell_value 2)))
		(not (numberp (cell_value 3)))
		(not (numberp (cell_value 4)))
		(not (numberp (cell_value 5)))
		(not (numberp (cell_value 6)))
		(not (numberp (cell_value 7)))
		(not (numberp (cell_value 8)))
		(not (numberp (cell_value 9)))
    )
)

;; play game
(defun put_marker()
    (loop
        (display_board)
        (check_win)
        (check_draw)
        (if (evenp counter)
            (format t "~A [X] " (string-upcase name1)) (format t "~A [O] " (string-upcase name2))
        )
        (format t "please enter position: ")
        (let ((choice (parse-integer (read-line) :junk-allowed t)))
            (cond
                ((not choice)
                    (progn
                        (format t "Invalid input!~%Please enter only integers from 1-9.~%")
                        (put_marker)
                    )
                )
                ((and(< choice 10)(> choice 0))
                    (check_marker choice)
                )
                ((not(and(< choice 10)(> choice 0)))
                    (progn
                        (format t "Invalid range!~%Please enter only integers from 1-9.~%")
                        (put_marker)
                    )
                )
            )
        )
    )
)

;; update marker positions
(defun check_marker(choice)
    (cond
        ((or (equal (cell_value choice) "X") (equal (cell_value choice) "O"))
            (progn
                (format t "Position has been taken. Please select another cell.~%")
                (force-output nil)
                (check_marker(put_marker))
            )
        )
        ((and (not (win_game)) (not (draw_game)))
            (setq actual_pos (- choice 1))
            (if (evenp counter)
                (setf (aref Board actual_pos) "X")
                (setf (aref Board actual_pos) "O")
            )
            (incf counter)
        )
    )
)

(defun check_win()
    (cond
        ((win_game)
        (progn
            (decf counter)
            (if (evenp counter) ; even = player1, odd = player2
                (format t "~A has won!" (string-upcase name1))
                (format t "~A has won!" (string-upcase name2))
            )
            (play_again)
        )
        )
    )
)

(defun check_draw()
    (cond
        ((draw_game)
            (progn
                (format t "Game tied!")
                (play_again)
            )
        )
    )
)

;; main function to start
(defun start_game()
    (format t "~%Welcome to Tic Tac Toe!")
    (setq counter 0)
    (player1)
    (player2)
    (format t "~%X (~A) will go first.~%~%" name1)
    (make_board)
    (put_marker)
)

(defun end_game()
    (format t "~%Thanks for playing, bye!")
    (abort)
)

(defun play_again()
    (format t "~%Rematch? [yes/no]: ")
    (setq answer(read))
    (if (string-equal answer "yes")
        (start_game)
        (end_game)
    )
)

(start_game)