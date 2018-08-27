(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (head z) (z 0))
(define (tail z) (z 1))

(define foo (cons "hello" "world"))

(head foo)
(tail foo)

(define (conz x y)
  (lambda (f) (f x y)))

(define (heaz z)
  (z (lambda (a b) a)))

(heaz (conz 1 2))
