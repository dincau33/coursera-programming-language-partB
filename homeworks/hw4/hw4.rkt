
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

; Problem 1: sequence
(define (sequence low high stride) 
  (if (>= high low)
  (cons low (sequence (+ low stride) high stride))
  (list)))

; Problem 2: string-append-map
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

; Problem 3: list-nth-mod
(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; Problem 4: stream-for-n-steps
(define (stream-for-n-steps s n)
  (let ([pr (s)])
  (if (= n 0)
  (list)
  (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

; Problem 5: funny-number-stream
(define (funny-number-stream) 
  (define (f x)
    (cons (if (= 0 (remainder x 5)) (- x) x) 
    (lambda() (f (+ x 1)))))
  (f 1))

; Problem 6: dan-then-dog
(define (dan-then-dog)
  (define (f x)
    (cons (if (even? x) "dog.jpg" "dan.jpg") 
    (lambda() (f (+ x 1)))))
  (f 1))

; Problem 7: stream-add-zero
(define (stream-add-zero s)
  (define (f x)
    (cons (cons 0 (car (x)))
          (lambda () (f (cdr (x))))))
  (lambda () (f s)))

; Problem 8: cycle-lists
(define (cycle-lists xs ys)
  (define (f n) 
    (cons (cons (list-nth-mod xs n) 
                (list-nth-mod ys n))
          (lambda () (f (+ n 1)))))
  (lambda () (f 0)))

; Problem 9: vector-assoc
(define (vector-assoc v vec)
  (define (f n)
    (if (>= n (vector-length vec))
      #f
      (let ([nth (vector-ref vec n)])
        (cond [(not (pair? nth)) (f (+ n 1))]
              [(equal? v (car nth)) nth]
              [#t (f (+ n 1))]))))
    (f 0))
  
; Problem 10: cached-assoc
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [slot 0]
           [f (lambda (x)
                  (let ([ans (vector-assoc x memo)])
                    (if ans
                      ans
                      (let ([new-ans (assoc x xs)])
                        (begin
                          (vector-set! memo slot new-ans)
                          (if (< slot (- n 1))
                            (set! slot (add1 slot))
                            (set! slot 0))
                          new-ans)))))])
  f))
