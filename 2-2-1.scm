(define (list-ref items n)
  (if (= 0 n)
      (car items)
      (list-ref (cdr items) (- n 1))))

(list-ref (list 5 6 7 8) 2)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(length (list 1 2 3 4))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(append (list 1 2) (list 3 4))

(define (last-pair items)
  (define (last-pair-iter h t)
    (if (null? t)
        (list h)
        (last-pair-iter (car t) (cdr t))))
  (last-pair-iter (car items) (cdr items)))

(define (last-pairz items)
  (if (null? (cdr items))
      (list (car items))
      (last-pairz (cdr items))))

(last-pairz (list))

(define (reverse items)
  (define (reverse-iter source tmp)
    (if (null? source)
        tmp
        (reverse-iter (cdr source) (cons (car source) tmp))))
  (reverse-iter items (list)))

(reverse (list 1 2 3 4))
