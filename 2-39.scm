(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op (car rest) result)
              (cdr rest))))
  (iter init seq))

(fold-left + 0 '(1 2 3 4))

(define (reverse seq)
  (fold-left (lambda (x y) (cons x y)) '() seq))

(reverse '(1 2 3 4))
