#lang racket
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
    (init-field [white-captured (make-vector 5 0))	; rook,knight,bishop,queen,pawn
(init-field [black-captured (make-vector 5 0))
(init-field (board
             (for/vector ([i '(1 2 3 4 5 6 7 8)])
               (for/vector ([j '(1 2 3 4 5 6 7 8)])
                 (new square% [pos (cons i j)])))))
(define/public (board-ref i j)
  (vector-ref (vector-ref board i) j))
(define/public (board-set! i j piece)
  (let* ([pos (board-ref i j)])
    (set-field! occupancy pos piece)))
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
                          (vector-set! black-captured 0 (+ 1 (vector-ref white-captured 0)))]
                         [(is-a? newpospiece knight%)
                          (vector-set! black-captured 1 (+ 1 (vector-ref white-captured 1)))]
                         [(is-a? newpospiece bishop%)
                          (vector-set! black-captured 2 (+ 1 (vector-ref white-captured 2)))]
                         [(is-a? newpospiece queen%)
                          (vector-set! black-captured 3 (+ 1 (vector-ref white-captured 3)))]
                         [(is-a? newpospiece pawn%)
                          (vector-set! black-captured 4 (+ 1 (vector-ref white-captured 4)))]))
               (board-set! i j piece))
        ; Otherwise, either promotion or just a move
        (if (is-a? piece pawn?)
            (if (and (equal? piececolor 'White) (= j 8))
                (let* ([opt (read)])
                  (
            
               
               
               
               
               
               
               
               