(define (filter predicate sequence)
  (cond ((null? sequence) sequence)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4))

(define (enumate-interval low high)
  (if (> low high)
      '()
      (cons low (enumate-interval (+ low 1) high))))

(enumate-interval 2 5)

(define (fringe tree)
  (cond ((null? tree) tree)
        ((not (pair? tree))
         (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

(fringe (list 1 (list 2 (list 3 (list 4)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              '()
              sequence))

(map (lambda (x) (* x 2)) (list 1 2 3))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2) (list 3 4))

(define (lenght seq)
  (accumulate (lambda (x y) (+ 1 y))  0 seq))

(lenght (list 1 2 3 4 5))
