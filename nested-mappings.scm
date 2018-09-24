(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (foo n)
  (filter prime-sum?
          (flatmap  (lambda (i)
                      (map (lambda (j) (list i j))
                           (enumerate-interval 1 (- i 1))))
                    (enumerate-interval 1 n))))

(foo 4)
