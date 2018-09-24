(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (permutations seq)
  (if (null? seq)
      (list seq)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x seq))))
               seq)))

(permutations (list 1 2 3))

(remove 1 (list 1 2))
