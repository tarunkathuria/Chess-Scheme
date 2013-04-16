#lang racket
(include "pieces.rkt")
(define (concat l) (foldr append `() l))

(define-syntax lc
  (syntax-rules (: <- @)
    [(lc exp : var <- lexp) (map (lambda (var) exp) lexp)]
    [(lc exp : @ guard) (if guard (list exp) `())]
    [(lc exp : @ guard qualifier ...) 
     (concat (lc (lc exp : qualifier ...) : guard))]
    [(lc exp : var <- lexp qualifier ...) 
     (concat (lc (lc exp :  qualifier ... ) : var <- lexp))]))

(define square%
  (class object%
    (init pos)
    (define position pos)
    (init-field (occupancy #f))
    (super-new)
    (define/public (occupied?)
      occupancy)))
; White goes from 1 to 8, black goes from 8 to 1
(define board%
  (class object%
    (super-new)
    (init-field [white-captured (make-vector 5 0)])	; rook,knight,bishop,queen,pawn
    (init-field [black-captured (make-vector 5 0)])
    ;teh actual board
    (init-field (board
                 (for/vector ([i '(1 2 3 4 5 6 7 8)])
                   (for/vector ([j '(1 2 3 4 5 6 7 8)])
                     (new square% [pos (cons i j)])))))
    ;Refer to a particular position on the board
    (define/public (board-ref i j)
      (vector-ref (vector-ref board i) j))
    ; Set on the i,j th position piece(or blank) 
    (define/public (board-set! i j piece)
      (let* ([pos (board-ref i j)])
        (set-field! occupancy pos piece)
        )) ;;;;;;;;;;;;What is curr-pos represented as?
    
    (define/public (print-board)
      (display board))
    
    ;Move piece to the new position i,j
    (define/public (move-piece! i j piece)
      (let* ([newpos (board-ref i j)]
             [piecepos (get-field curr-pos piece)]
             [piececolor (get-field color piece)]
             [newpospiece (get-field occupancy newpos)]
             [nppcolor (if newpospiece (get-field color newpospiece)
                           #f)])
        ; If piece in new pos, then obviously it is going to be elim.
        (if newpospiece
            (begin (if (equal? nppcolor 'White)
                       (cond [(is-a? newpospiece rook%)
                              (vector-set! white-captured 0 (+ 1 (vector-ref white-captured 0)))]
                             [(is-a? newpospiece knight%)
                              (vector-set! white-captured 1 (+ 1 (vector-ref white-captured 1)))]
                             [(is-a? newpospiece bishop%)
                              (vector-set! white-captured 2 (+ 1 (vector-ref white-captured 2)))]
                             [(is-a? newpospiece queen%)
                              (vector-set! white-captured 3 (+ 1 (vector-ref white-captured 3)))]
                             [(is-a? newpospiece pawn%)
                              (vector-set! white-captured 4 (+ 1 (vector-ref white-captured 4)))])
                       (cond [(is-a? newpospiece rook%)
                              (vector-set! black-captured 0 (+ 1 (vector-ref black-captured 0)))]
                             [(is-a? newpospiece knight%)
                              (vector-set! black-captured 1 (+ 1 (vector-ref black-captured 1)))]
                             [(is-a? newpospiece bishop%)
                              (vector-set! black-captured 2 (+ 1 (vector-ref black-captured 2)))]
                             [(is-a? newpospiece queen%)
                              (vector-set! black-captured 3 (+ 1 (vector-ref black-captured 3)))]
                             [(is-a? newpospiece pawn%)
                              (vector-set! black-captured 4 (+ 1 (vector-ref black-captured 4)))]))
                   ;now set on the board's ijth pos that piece
                   (board-set! i j piece)
                   (board-set! (car piecepos) (cdr piecepos) #f)
                   (set-field! curr-pos (cons i j) piece));;;;;Is there anything else?       
            ; Otherwise, either promotion or just a move;;Enp and castling to be done!!!!!!
            (if (is-a? piece pawn?)
                (when (and (equal? piececolor 'White) (= j 8))
                  (let* ([opt (read)])
                    (cond [(equal? opt 'Queen) (begin (board-set! i j (new queen% [curr-pos (cons i j)] [color (get-field col piece)])))]
                          [(equal? opt 'Rook) (begin (board-set! i j (new rook% [curr-pos (cons i j)] [color (get-field col piece)])))]
                          [(equal? opt 'Bishop) (begin (board-set! i j (new bishop% [curr-pos (cons i j)] [color (get-field col piece)])))]
                          [(equal? opt 'Knight) (begin (board-set! i j (new knight% [curr-pos (cons i j)] [color (get-field col piece)])))])
                    (board-set! (car piecepos) (cdr piecepos) #f)))
                (begin (board-set! i j piece)
                       (board-set! (car piecepos) (cdr piecepos) #f)
                       (set-field! curr-pos (cons i j) piece))))))
    (define/public (initialise)
      (define (helper i j)
        (cond[(= i 1)
              (cond [(= j 1) (board-set! i j (new rook% [curr-pos (cons i j)] [color 'Black]))]
                    [(= j 2) (board-set! i j (new knight% [curr-pos (cons i j)] [color 'Black]))]
                    [(= j 1) (board-set! i j (new rook% [curr-pos (cons i j)] [color 'Black]))]
                    [(= j 1) (board-set! i j (new rook% [curr-pos (cons i j)] [color 'Black]))]
                    [(= j 1) (board-set! i j (new rook% [curr-pos (cons i j)] [color 'Black]))])]
             [(= i 8)
              (cond [(= j 1) (board-set! i j (new rook% [curr-pos (cons i j)] [color 'White]))]
                    [(= j 2) (board-set! i j (new knight% [curr-pos (cons i j)] [color 'White]))]
                    [(= j 1) (board-set! i j (new rook% [curr-pos (cons i j)] [color 'White]))]
                    [(= j 1) (board-set! i j (new rook% [curr-pos (cons i j)] [color 'White]))]
                    [(= j 1) (board-set! i j (new rook% [curr-pos (cons i j)] [color 'White]))])]
             [(= i 2) (board-set! i j (new pawn% [curr-pos (cons i j)] [color 'Black]))]
             [(= i 7) (board-set! i j (new pawn% [curr-pos (cons i j)] [color 'White]))]
             [else (board-set! i j #f)]))
      (for-each (λ(i) (for-each (λ(j) (helper i j)) '(1 2 3 4 5 6 7 8))) '(1 2 3 4 5 6 7 8)))
    (define/public (make-move!)
      (let* ([posPx (display "InitialPositionX") (newline) (read)]
             [posPy (display "InitialPositionY") (newline) (read)]
             [posNx (display "FinalPositionX") (newline) (read)]
             [posNy (display "FinalPositionX") (newline) (read)]
             [piece (get-field occupancy (board-ref posPx posPy))]
             [validMovesList (send piece valid-Move)])
        (if (
        
    
    
    
    
    
    
    
    
    
    
    
    
    