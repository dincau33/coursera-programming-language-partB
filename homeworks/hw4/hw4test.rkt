#lang racket
;; Programming Languages Homework4 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and change HOMEWORK_FILE to the name of your homework file.
(require "hw4.rkt")

(require rackunit)

;; Helper functions
(define ones (lambda () (cons 1 ones)))
(define a 2)
(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

(define tests
  (test-suite
   "Sample tests for Assignment 4"
   
   ; sequence test
   (check-equal? (sequence 0 5 1) (list 0 1 2 3 4 5) "Sequence test1_1")
   (check-equal? (sequence 3 11 2) (list 3 5 7 9 11) "Sequence test1_2")
   (check-equal? (sequence 3 8 3) (list 3 6) "Sequence test1_3")
   (check-equal? (sequence 3 2 1) (list) "Sequence test1_4")
   (check-equal? (sequence 3 3 1) (list 3) "Sequence test1_5")

   ; string-append-map test
   (check-equal? (string-append-map (list "dan" "dog" "curry" "dog2") ".jpg") '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg") "string-append-map test2_1")
   (check-equal? (string-append-map (list) ".jpg") (list) "string-append-map test2_2")
   
   ; list-nth-mod test
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 2) 2 "list-nth-mod test3_1")
   (check-equal? (list-nth-mod (list 6 1 2 3 4) 0) 6 "list-nth-mod test3_2")
   (check-equal? (list-nth-mod (list "a" "b") 0) "a" "list-nth-mod test3_3")
   (check-equal? (list-nth-mod (list 6 1 2 3 4) 20) 6 "list-nth-mod test3_4")
   (check-exn exn:fail? (lambda () (list-nth-mod (list 0 1 2 3 4) -2)))
   (check-exn exn:fail? (lambda () (list-nth-mod (list) 2)))
   (check-exn exn:fail? (lambda () (list-nth-mod (list) -2)))
   
   ; stream-for-n-steps test
   (check-equal? (stream-for-n-steps ones 2) (list 1 1) "stream-for-n-steps test4_1")
   (check-equal? (stream-for-n-steps ones 0) (list) "stream-for-n-steps test4_2")
   (check-equal? (stream-for-n-steps ones 1) (list 1) "stream-for-n-steps test4_3")
   (check-equal? (stream-for-n-steps powers-of-two 0) (list) "stream-for-n-steps test4_4")
   (check-equal? (stream-for-n-steps powers-of-two 1) (list 2) "stream-for-n-steps test4_5")
   (check-equal? (stream-for-n-steps powers-of-two 2) (list 2 4) "stream-for-n-steps test4_6")
   (check-equal? (stream-for-n-steps powers-of-two 3) (list 2 4 8) "stream-for-n-steps test4_7")
   
   ; funny-number-stream test
   (check-equal? (stream-for-n-steps funny-number-stream 16) (list 1 2 3 4 -5 6 7 8 9 -10 11 12 13 14 -15 16) "funny-number-stream test5_1")
   (check-equal? (stream-for-n-steps funny-number-stream 0) (list) "funny-number-stream test5_2")
   (check-equal? (stream-for-n-steps funny-number-stream 1) (list 1) "funny-number-stream test5_3")
   
   ; dan-then-dog test
   (check-equal? (stream-for-n-steps dan-then-dog 1) (list "dan.jpg") "dan-then-dog test6_1")
   (check-equal? (stream-for-n-steps dan-then-dog 2) (list "dan.jpg" "dog.jpg") "dan-then-dog test6_2")
   (check-equal? (stream-for-n-steps dan-then-dog 0) (list) "dan-then-dog test6_3")
   (check-equal? (stream-for-n-steps dan-then-dog 3) (list "dan.jpg" "dog.jpg" "dan.jpg") "dan-then-dog test6_4")
   
   ; stream-add-zero test
   (check-equal? (stream-for-n-steps (stream-add-zero ones) 1) (list (cons 0 1)) "stream-add-zero test7_1")
   (check-equal? (stream-for-n-steps (stream-add-zero ones) 0) (list) "stream-add-zero test7_2")
   (check-equal? (stream-for-n-steps (stream-add-zero ones) 2) (list (cons 0 1) (cons 0 1)) "stream-add-zero test7_3")
   (check-equal? (stream-for-n-steps (stream-add-zero dan-then-dog) 1) (list (cons 0 "dan.jpg")) "stream-add-zero test7_4")
   (check-equal? (stream-for-n-steps (stream-add-zero dan-then-dog) 2) (list (cons 0 "dan.jpg") (cons 0 "dog.jpg")) "stream-add-zero test7_5")
   (check-equal? (stream-for-n-steps (stream-add-zero powers-of-two) 1) (list (cons 0 2)) "stream-add-zero test7_6")
   (check-equal? (stream-for-n-steps (stream-add-zero powers-of-two) 2) (list (cons 0 2) (cons 0 4)) "stream-add-zero test7_7")
   
   ; cycle-lists test
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 3) (list (cons 1 "a") (cons 2 "b") (cons 3 "a")) "cycle-lists test8_1")
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a")) 3) (list (cons 1 "a") (cons 2 "a") (cons 3 "a")) "cycle-lists test8_2")
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b" "c")) 3) (list (cons 1 "a") (cons 2 "b") (cons 3 "c")) "cycle-lists test8_3")
   
   ; vector-assoc test
   (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) (cons 4 1) "vector-assoc test9_1")
   (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 4 13))) (cons 4 1) "vector-assoc test9_2")
   (check-equal? (vector-assoc 1 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) #f "vector-assoc test9_3")
   (check-equal? (vector-assoc "benoit" (vector (cons "benoit" 10) (cons "a" 1) (cons "b" 1) (cons "c" 1))) (cons "benoit" 10) "vector-assoc test9_4")
   (check-equal? (vector-assoc 4 (vector 2 (cons 3 1) (cons 4 1) 4)) (cons 4 1) "vector-assoc test9_5")
   (check-equal? (vector-assoc 4 (vector)) #f "vector-assoc test9_6")
   
   ; cached-assoc tests
   (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 3) (cons 3 4) "cached-assoc test")
   
   ; while-less test
   ; (check-equal? (while-less 7 do (begin (set! a (+ a 1)) a)) #t "while-less test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
