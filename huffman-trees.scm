;; generic procedures
(define (make-leaf symbol w)
  (list 'leaf symbol w))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

;;decoding
(define (choose-branch bit branch)
  (cond
   ((= bit 0) (left-branch branch))
   ((= bit 1) (right-branch branch))
   (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

;; sets of weights
(define (adjoint-set x set)
  (cond
   ((null? set) (list x))
   ((< (weight x) (weight (car set)))
    (cons x set))
   (else (cons (car set)
               (adjoint-set x (cdr set))))))

;; this method expects pairs of the like: ((A 4) (B 2) (C 1) (D 1))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoint-set (make-leaf (car pair)
                                (cadr pair))
                     (make-leaf-set (cdr pairs))))))

;; Huffman tree generator
(define (generate-huffman-tree pairs)
  (define (successive-merge pairs)
    (if (null? (cdr pairs))
        (car pairs)
        (successive-merge
         (adjoint-set (make-code-tree (car pairs)
                                      (cadr pairs))
                      (cddr pairs)))))
  (successive-merge (make-leaf-set pairs)))

;; example
(decode '(0 1 1 0 0 1 0 1 0 1 1 1 0)
        (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))
