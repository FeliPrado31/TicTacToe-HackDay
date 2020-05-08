;;;; board.lisp
;;;; 
;;;; Juan Felipe Cubillos Prado, Samir Millan orozco, David Ortiz, Orlando Gomez Lopez
;;;; 08/05/2020


;;; Def player *Player* 1 (Player 1) X
;;; Def player *Player2* 10 (Player 2) O
;;; Def player *computer* 10 (computer) O
(defvar *player* 1)
(defvar *computer* 10)
(defvar *player2* 10)
;;; Create board
(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))
;;; Switch player depend select play
(defun convert-to-letter (v)
  (cond ((eql v 1) "O")
        ((eql v 10) "X")
        (t " ")))
;;; Define the rows to use X,Y,Z
(defun print-row (x y z)
  (format t "~&  ~A | ~A | ~A"
          (convert-to-letter x)
          (convert-to-letter y)
          (convert-to-letter z)))
;;; Print the board 123,345,789.
(defun print-board (board)
  (format t "~%")
  (print-row (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~& -----------")
  (print-row (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~& -----------")
  (print-row (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~%~%"))
;;; Play a move depend player X or O
(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)
;;; We define the number of possibilities.
;;; It also defines if a player wins, 
(defvar *triplets*
  '((1 2 3) (4 5 6) (7 8 9) ; Horizontal
    (1 4 7) (2 5 8) (3 6 9) ; Vertical
    (1 5 9) (3 5 7)))       ; Diagonal
;;; Defines the sum of the positions
;;; To determine if you have won
(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))
;;; Let's capture the elements
;;; Mapcar takes the elements from one another. to form a list
;;; Final Mind Lamp allows us to take this list
;;; Operate it and return its result
(defun compute-sums (board)
  (mapcar #'(lambda (triplet)
              (sum-triplet board triplet))
          *triplets*))
;;; This function is called every time someone
;;; selects a cell, to make displaying the winner
(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *player*) sums)
        (member (* 3 *computer*) sums)
        (member (* 3 *player2*) sums))))
;;; 
(defun board-full-p (board)
  (not (member 0 board)))
;;; This function will determine who moves on a turn.
;;; Terms (Invalip input.)
;;; Terms (That space is already occupied.)
(defun read-a-legal-move (board)
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
                     (<= 1 pos 9)))
           (format t "~&Invalid input.")
           (read-a-legal-move board))
          ((not (zerop (nth pos board)))
           (format t "~&That space is already occupied.")
           (read-a-legal-move board))
          (t pos))))
;;; This function generates a value to be assigned when called.
;;; This value is generated only if the position has no value,
;;; and is determined by the ZEROP function (zerop 0) => true.
(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
      pos(pick-random-empty-position board))))
;;; This function determines that a space is empty.
;;; Making use of the ZEROP function (zerop 0) => true
(defun find-empty-position (board squares)
  (find-if #'(lambda (pos)
               (zerop (nth pos board)))
           squares))
;;; They determine which block is complete, 
;;; making use of the function sum-triplet.
(defun win-or-block (board target-sum)
  (let ((triplet (find-if
                   #'(lambda (trip)
                       (equal (sum-triplet board trip) target-sum))
                   *triplets*)))
    (when triplet
      (find-empty-position board triplet))))
;;; Movement depending on whether the space is empty
(defun random-move-strategy (board)
  (list (pick-random-empty-position board)
        "Random move"))
;;; Determine whether to block or win
(defun make-three-in-a-row (board)
  (let ((pos (win-or-block board (* 2 *computer*))))
    (and pos (list pos "Try to make three in a row"))))
;;; Determine whether to block or win
(defun block-player-win (board)
  (let ((pos (win-or-block board (* 2 *player*))))
    (and pos (list pos "Blocking player win"))))
;;; Select whether to block, win or occupy an empty position
(defun choose-best-move (board)
  (or (make-three-in-a-row board)
      (block-player-win board)
      (random-move-strategy board)))
;;; Computer move
(defun computer-move (board)
  "Placeholder"
  board)
;;; Terms (Invalip input.)
;;; Terms (That space is already occupied.)
;;; Called make-move
;;; Print new value in board
;;; Finally determine if you win or not during the same turn
;;; or tiee game.
(defun player-move (board)
  (format t "~&Your move: ")
  (let* ((pos (read-a-legal-move board))
         (new-board (make-move *player* pos board)))
    (print-board new-board)
    (cond ((winner-p new-board) (format t "~&You win!"))
          ((board-full-p new-board) (format t "~&Tie game."))
          (t (computer-move new-board)))))

(defun player-move1 (board)
  (format t "~&Your move: ")
  (let* ((pos (read-a-legal-move board))
         (new-board (make-move *player* pos board)))
    (print-board new-board)
    (cond ((winner-p new-board) (format t "~&You win!"))
          ((board-full-p new-board) (format t "~&Tie game."))
          (t (player2-move new-board)))))

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
         (pos (first best-move))
         (strategy (second best-move))
         (new-board (make-move *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~S" strategy)
    (print-board new-board)
    (cond ((winner-p new-board) (format t "~&I win!"))
          ((board-full-p new-board) (format t "~&Tie game."))
          (t (player-move new-board)))))

  (defun player2-move (board)
  (let* ((pos (read-a-legal-move board))
         (new-board (make-move *player2* pos board)))
    (print-board new-board)
    (cond ((winner-p new-board) (format t "~&Player 2 win!"))
          ((board-full-p new-board) (format t "~&Tie game."))
          (t (player-move1 new-board)))))
;;; Select option play with other player or computer.
(defun play ()
  (if (y-or-n-p "Do you wanna play vs computer?")
    (if (y-or-n-p "Would you like to play first?")
      (player-move (make-board))
      (computer-move (make-board)))
    (if (y-or-n-p "Would you like to play first player1?")
      (player-move1 (make-board))
      (player2-move (make-board)))))
(play)