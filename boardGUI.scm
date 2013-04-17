;(require "guitest.scm")
;Makes board on GUI
;Self contained
(define (imgAt y x tagStr)
  (let* ([sqState (get-field occupancy (send board board-ref x y))])
    (if (not sqState)
        (if (even? (+ x y))
            "Images/white.png"
            "Images/black.png")
        (cond [(is-a? sqState rook%) (string-append "Images/" 
                                                    (if (even? (+ x y)) "white" "black") 
                                                    (if (equal? (get-field color sqState) 'Black) "-B-" "-W-")
                                                    "rook" tagStr ".png")]
              [(is-a? sqState knight%) (string-append "Images/" 
                                                    (if (even? (+ x y)) "white" "black") 
                                                    (if (equal? (get-field color sqState) 'Black) "-B-" "-W-")
                                                    "knight" tagStr ".png")]
              [(is-a? sqState pawn%) (string-append "Images/" 
                                                    (if (even? (+ x y)) "white" "black") 
                                                    (if (equal? (get-field color sqState) 'Black) "-B-" "-W-")
                                                    "pawn" tagStr ".png")]
              [(is-a? sqState queen%) (string-append "Images/" 
                                                    (if (even? (+ x y)) "white" "black") 
                                                    (if (equal? (get-field color sqState) 'Black) "-B-" "-W-")
                                                    "queen" tagStr ".png")]
              [(is-a? sqState king%) (string-append "Images/" 
                                                    (if (even? (+ x y)) "white" "black") 
                                                    (if (equal? (get-field color sqState) 'Black) "-B-" "-W-")
                                                    "king" tagStr".png")]
              [(is-a? sqState bishop%) (string-append "Images/" 
                                                    (if (even? (+ x y)) "white" "black") 
                                                    (if (equal? (get-field color sqState) 'Black) "-B-" "-W-")
                                                    "bishop" tagStr ".png")]))))

(define (drawValidMoves board posn)
  (let* ([piece (get-field occupancy (board-ref (car posn) (cdr posn)))]
         [pieceValidMoves (send piece valid-move)])
    (for-each (imgAt (car posn) (cdr posn) "-possible-move") pieceValidMoves)))
    
    


(define (make-board)
  (define (makeRow i j)
    (define x (+ 1 (/ (- j horiz-inset) imgWidth)))
    (define y (+ 1 (/ (- i vert-inset) imgHeight)))
    (if (< j (+ horiz-inset (- width imgWidth)))
        (begin
          ((draw-pixmap Chess-Window) (imgAt x y "") (make-posn j i) (make-rgb 0 0 0))
          (makeRow i (+ j imgWidth)))
          ((draw-pixmap Chess-Window) (imgAt x y "") (make-posn j i) (make-rgb 0 0 0))))
  (define (makeMultRows i)
    (if (< i (- (+ height vert-inset) imgHeight))
        (begin (makeRow i horiz-inset)
               (makeMultRows (+ i imgHeight)))
        (makeRow i horiz-inset)))
  (makeMultRows vert-inset))
(define temp1 (cons 0 0))
(define temp2 (cons 0 0))
(define temp3 1)
(define (movesMCL)
  (define posn (mouse-click-posn (get-mouse-click Chess-Window))) 
   (define y (+ 1 (quotient (- (posn-x posn) horiz-inset) imgWidth)))
  (define x (+ 1 (quotient (- (posn-y posn) vert-inset) imgHeight)))
  (display x) (newline) (display y) (newline)
  (if (and (odd? temp3) 
           (>= (posn-x posn) horiz-inset) (>= (posn-y posn) vert-inset) 
           (>= x 1) (>= y 1) (<= x 8) (<= y 8)  (not (get-field occupancy (send board board-ref x y))))
      (begin (display "1") (movesMCL))
      (if (and (odd? temp3)   
           (>= (posn-x posn) horiz-inset) (>= (posn-y posn) vert-inset) 
           (>= x 1) (>= y 1) (<= x 8) (<= y 8) (not (equal? turn (get-field color (get-field occupancy (send board board-ref x y))))))
        (begin (display "2") (movesMCL))
      (if (and (odd? temp3)  
           (>= (posn-x posn) horiz-inset) (>= (posn-y posn) vert-inset) 
           (>= x 1) (>= y 1) (<= x 8) (<= y 8) (equal? turn (get-field color (get-field occupancy (send board board-ref x y)))) )
          (begin
            (display "3")
            ((draw-pixmap Chess-Window) (imgAt x y "-selected") 
                                        (make-posn (+ horiz-inset (* imgWidth (- x 1))) (+ vert-inset (* imgHeight (- y 1)))) 
                                        (make-rgb 0 0 0))
            
            (set! temp1 (cons y x))
            ;(set! temp3 (+ 1 temp3))
            (movesMCL)) 
         ; (movesMCL)
          (begin (display x) (newline) (display y) (newline) (movesMCL))
          ))))
          
(define (play)
  (make-board)
  (movesMCL))
      
