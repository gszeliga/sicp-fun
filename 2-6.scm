;; Church numerals
(define zero
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (inc n)
  (+ n 1))

((zero inc) 2)

(define one (add-1 zero))
(define two (add-1 one))

(define onez
  (lambda (f) (lambda (x) (f x))))

(define twoz
  (lambda (f) (lambda (x) (f (f x)))))

((onez inc) 1)
((twoz inc) 2)

(define (add-church n m)
  (lambda (f) (lambda (x) ((n f) ((m f) x)))))

(define four (add-church two two))

((four inc) 4)
