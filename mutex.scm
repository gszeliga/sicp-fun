(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialize-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialize-p)))

(define (make-mutex)
  (let ((cell (list #f)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             ; 'true' means not-acquired here
             (if (test-and-set! cell)
                 ; we try again recursively
                 (the-mutex m)))
            ((eq? m 'release)
             (clear! cell))))
    the-mutex))

(define (test-and-set! cell)
  (if (car cell)
      #t
      (begin
        (set-car! cell true)
        #f)))

(define (clear! cell)
  (set-car! cell #f))

;; 3.47.a implementation of a semaphore in terms of mutexes
(define (make-semaphore n)
  (let ((slots-mutex (make-mutex))
        (slots n))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (begin              
               (slots-mutex 'acquire)
               (if (> slots 0)
                   (begin
                     (set! slots (- slots 1))
                     (slots-mutex 'release))
                   (begin
                     (slots-mutex 'release)
                     (the-semaphore m)))))
            ((eq? m 'release)
             (begin
               (slot-mutex 'acquire)             
               (if (< slots n)
                   (begin
                     (set! slots (+ slots 1))
                     (slot-mutex 'release))
                   (slot-mutex 'release))))))
    the-semaphore))

;; 3.47.b implementation of a semaphore in terms of test-and-set!
(define (make-semaphore n)
  (let ((cell (list #t))
        (slots n))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-semaphore m)
                 (if (zero? slots)
                     (begin
                       (clear! cell)
                       (the-semaphore m))
                     (begin
                       (set! slots (- slots 1))
                       (clear! cell)))))
            ((eq? m 'release)
             (if (test-and-set! cell)
                 (the-semaphore m)
                 (if (< slots n)
                     (begin
                       (slots (+ slots 1))
                       (clear! cell))
                     (clear! cell))))))
    the-semaphore))
