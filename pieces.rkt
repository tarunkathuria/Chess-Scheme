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

(define board%
  (class object%
    (super-new)
    (init-field (board
                 (for/vector ([i '(1 2 3 4 5 6 7 8)])
                   (for/vector ([j '(1 2 3 4 5 6 7 8)])
                     (new square% [pos (cons i j)])))))))



(define piece%
  (class object%
    (super-new)
    (init-field color)
    (init-field [curr-pos (void)])
    (define/public (valid-move) (display "override"))))

;;;;;Pieces;;;;;
;King
(define king%
  (class piece%
    (init col)
    (super-new [color col])
   
    (define/override (valid-move)0)))

;Queen
(define queen%
  (class piece%
    (init col)
    (super-new [color col])
    (set-field! curr-pos this (if (equal? (get-field color this) 'black)
                                  (cons 1 5)
                                  (cons 8 4)))
    (define/override (valid-move)
      0)))

;Bishop
(define bishop%
  (class piece%
    (init col)
    (super-new [color col])
    (define/override (valid-move)0)))
