(include "pieces.scm")
(include "board.scm")

(include "syntactic-sugar.scm")

(send board print-board)
(get-field color (get-field occupancy (send board board-ref 1 3)))
(send board make-move!)

(send board print-board)
