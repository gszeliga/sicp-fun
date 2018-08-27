(define (linear-recursive-factorial n)
  (if (= 1 n)
      1
      (* n (factorial (- n 1)))))

(linear-recursive-factorial 9)


(define (iter-factorial n)
  (define (iter product times)
    (if (> times n)
        product
        (iter (* product times)
              (+ 1 times))))
  (iter 1 1))

(iter-factorial 9)
