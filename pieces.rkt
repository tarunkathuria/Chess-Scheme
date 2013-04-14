#lang racket
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
    (define/override (valid-move)
      (define posx (car (get-field curr-pos this)))
      (define posy (cdr (get-field curr-pos this)))
      (mbound (list
               (cons (+ posx 1) posy)
               (cons (- posx 1) posy)
               (cons posx (+ posy 1))
               (cons posx (- posy 1))
               (cons (+ posx 1) (+ posy 1))
               (cons (+ posx 1) (- posy 1))
               (cons (- posx 1) (+ posy 1))
               (cons (- posx 1) (- posy 1)))))))

;Queen
(define queen%
  (class piece%
    (init col)
    (super-new [color col])
    (define/override (valid-move)
      (define posx (car (get-field curr-pos this)))
      (define posy (cdr (get-field curr-pos this)))
      (mbound (append
               (lc (cons posx y) : y <- (one-to-n 8) @ (not(= y posy)))
               (lc (cons x posy) : x <- (one-to-n 8) @ (not (= x posx)))
               (lc (cons x y) : x <- (one-to-n 8) y <- (one-to-n 8) @ 
                   (and (= (abs (- x y)) (abs (- posx posy))) 
                        (not (and (equal? x posx) (equal? y posy))))))))))

;Bishop
(define bishop%
  (class piece%
    (init col)
    (super-new [color col])
    (define/override (valid-move)
      (define posx (car (get-field curr-pos this)))
      (define posy (cdr (get-field curr-pos this)))
      (mbound (lc (cons x y) : x <- (one-to-n 8) y <- (one-to-n 8) @ 
                  (and (= (abs (- x y)) (abs (- posx posy))) 
                       (not (and (equal? x posx) (equal? y posy)))))))))

;Rook
(define rook%
  (class piece%
    (init col)
    (super-new [color col])
    (define/override (valid-move)
      (define posx (car (get-field curr-pos this)))
      (define posy (cdr (get-field curr-pos this)))
      (mbound (append
               (lc (cons posx y) : y <- (one-to-n 8) @ (not(= y posy)))
               (lc (cons x posy) : x <- (one-to-n 8) @ (not (= x posx))))))))

;Pawn
(define pawn%
  (class piece%
    (init col)
    (super-new [color col])
    (define/override (valid-move)
      (define posx (car (get-field curr-pos this)))
      (define posy (cdr (get-field curr-pos this))) ;;Attack move/ starting 2 move
      (mbound (cons posx (+ 1 posy))))))

;Knight
(define knight%
  (class piece%
    (init col)
    (super-new [color col])
    (define/override (valid-move)
      (define posx (car (get-field curr-pos this)))
      (define posy (cdr (get-field curr-pos this)))
      (mbound (list
               (cons (+ 1 posx) (+ 2 posy))
               (cons (+ 1 posx) (- 2 posy))
               (cons (- 1 posx) (+ 2 posy))
               (cons (- 1 posx) (- 2 posy))
               (cons (+ 2 posx) (+ 1 posy))
               (cons (+ 2 posx) (- 1 posy))
               (cons (- 2 posx) (+ 1 posy))
               (cons (- 2 posx) (- 1 posy)))))))

;;;;;The functions that bounds a piece inside the board;;;;;
(define (mbound l)
  (define (helper l fl)
    (if (null? l) fl
        (cond [(or (< (caar l) 1)
                   (< (cdar l) 1)
                   (> (caar l) 8)
                   (> (cdar l) 8))
               (helper (cdr l) fl)]
              [else (helper (cdr l) (append fl (list (car l))))])))
  (helper l '()))

;;;;;Definitions;;;;;
(define (concat l) (foldr append `() l))

(define-syntax lc
  (syntax-rules (: <- @)
    [(lc exp : var <- lexp) (map (lambda (var) exp) lexp)]
    [(lc exp : @ guard) (if guard (list exp) `())]
    [(lc exp : @ guard qualifier ...) 
     (concat (lc (lc exp : qualifier ...) : guard))]
    [(lc exp : var <- lexp qualifier ...) 
     (concat (lc (lc exp :  qualifier ... ) : var <- lexp))]))

(define (one-to-n n)
  (if (= n 0) `()
      (append (one-to-n (- n 1)) (list n))))
