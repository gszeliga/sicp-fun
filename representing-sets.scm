;; 2.59 unordered sets
;;
(define (element-of-set? x set)
  (cond
   ((null? set) #f)
   ((equal? x (car set)) #t)
   (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond
   ((or (null? set1) (null? set2)) '())
   ((element-of-set? (car set1) set2)
    (cons (car set1) (intersection-set (cdr set1) set2)))
   (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond
   ((null? set1) set2)
   ((not (element-of-set? (car set1) set2))
    (cons (car set1) (union-set (cdr set1) set2)))
   (else (union-set (cdr set1) set2))))

(define (union-set1 set1 set2)
  (if (null? set1)
      set2
      (union-set1 (cdr set1)
                  (adjoin-set (car set1) set2))))

;; 2.60 ordered sets
(define (element-of-set? x set)
  (cond
   ((null? set) #f)
   ((> x (car set)) #f)
   ((= x (car set)) #t)
   (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond
         ((< x1 x2) (intersection-set (cdr set1) set2))
         ((> x1 x2) (intersection-set set1 (cdr set2)))
         (else (cons x1 (intersection-set (cdr set1) (cdr set2))))))))

(define (union-set set1 set2)
  (cond
   ((null? set1) set2)
   ((null? set2) set1)
   (else (let ((x1 (car set1))
               (x2 (car set2)))
           (cond
            ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
            ((> x1 x2) (cons x2 (union-set set1 (cdr set2))))
            (else (cons x1 (union-set (cdr set1) (cdr set2)))))))))

(adjoin-set 1 '(1))

(intersection-set '(1 2 3 8) '(2 3 4 7 8))

(union-set '(1 3 8 9) '(2 4 10))

; car of cdr
(cadr '(a (b c d) (e f)))

; cdr of cdr
(caddr '(a (b c d) (e f)))
