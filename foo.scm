(define (square x) (* x x))

(square 4)

(define (sum-of-squares x y)
  (+ (square x)
     (square y)))

(sum-of-squares 2 3)

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(abs 10)

(define (max-of-squares a b c)
  (cond ((and (> a b) (> b c)) (sum-of-squares a b))
        ((and (> b a) (> c a)) (sum-of-squares b c))
        (else (sum-of-squares c a))))

(max-of-squares 4 2 3)
