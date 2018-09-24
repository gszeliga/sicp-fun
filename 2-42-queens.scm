;; (
;;  ((queen-1)(queen-2)(queen-3)...(queen-n)) --> We build all possible permutations where to put each queen 
;;  ((queen-1)(queen-2)(queen-3)...(queen-n))
;; )

;; The idea is to add a new column on each iteration and put a queen on each new row,then, we verify which from
;; all the generated permitations are still 'safe', meaining, no queen is under attack

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (make-position row col)
  (cons row col))

(define (n-col pos)
  (cdr pos))

(define (n-row pos)
  (car pos))

;;(define (safe? kth-position positions) #t)

(define (safe? col positions)
   (let ((kth-queen (list-ref positions (- col 1)))
         (other-queens (filter (lambda (q)
                                 (not (= col (n-col q))))
                               positions)))
   (define (attacks? q1 q2)
     (or (= (n-row q1) (n-row q2))
         (= (abs (- (n-row q1) (n-row q2)))
            (abs (- (n-col q1) (n-col q2))))))

   (define (iter q board)
     (or (null? board)
         (and (not (attacks? q (car board)))
              (iter q (cdr board)))))
   (iter kth-queen other-queens)))

(define (adjoin-position new-row col rest)
  (append rest (list (make-position new-row col))))

(define (empty-board) null)

(define (queens board-size)
  (define (queen-cols k) ;; k is the current column...really bad name
    (if (= k 0)
        (list '())
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)
