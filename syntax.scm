(define-syntax for
  (syntax-rules (:)
    [(for init : condition : incr : statement)
     
     (letrec ((loop (lambda ()
                      (cond [condition (begin
                                         statement
                                         incr
                                         (loop))]))))
       (begin init (loop)))]))