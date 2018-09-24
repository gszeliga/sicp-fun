(define (same-parity first . rest)
  (define (should-add? x)
    (= (modulo first 2) (modulo x 2)))
  
  (define (iter v tmp)
    (if (null? v)
        tmp
        (iter (cdr v) (if (should-add? (car v)) 
                          (cons (car v) tmp)
                          tmp)
              )))
  (iter rest (list first)))

(same-parity 7 1 2 3 10)
(same-parity 10 2 3 4 5)

(cons 1 (cons 2 (cons 3 (cons 5 (list)))))
