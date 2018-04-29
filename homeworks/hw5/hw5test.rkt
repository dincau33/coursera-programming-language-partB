#lang racket
;; Programming Languages Homework 5 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and, if necessary, change the filename
(require "hw5.rkt")

(require rackunit)

(define tests
  (test-suite
   "Sample tests for Assignment 5"
   
   ;; check racketlist to mupllist with normal list
   (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (aunit))) "racketlist->mupllist test1")
   (check-equal? (racketlist->mupllist (list (int 3))) (apair (int 3) (aunit)) "racketlist->mupllist test2")
   (check-equal? (racketlist->mupllist (list)) (aunit) "racketlist->mupllist test3")
   
   ;; check mupllist to racketlist with normal list
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit)))) (list (int 3) (int 4)) "racketlist->mupllist test1")
   (check-equal? (mupllist->racketlist (apair (int 3) (aunit))) (list (int 3)) "racketlist->mupllist test2")
   (check-equal? (mupllist->racketlist (aunit)) (list) "racketlist->mupllist test3")

   ;; var test
   (check-equal? (eval-under-env (var "test") (list (cons "test" (int 3)))) (int 3) "var test")

   ;; int test
   (check-equal? (eval-exp (int 3)) (int 3) "int test")
   
   ;; ifgreater tests
   (check-equal? (eval-exp (ifgreater (int 3) (int 4) (int 3) (int 2))) (int 2) "ifgreater test1")
   (check-equal? (eval-exp (ifgreater (int 4) (int 3) (int 3) (int 2))) (int 3) "ifgreater test2")
   (check-equal? (eval-exp (ifgreater (int 3) (int 3) (int 3) (int 2))) (int 2) "ifgreater test3")
   (check-exn exn:fail? (lambda () (eval-exp (ifgreater (aunit) (int 3) (int 3) (int 2)))))

   ;; fun test2
   (check-equal? (eval-exp (fun #f "x" (aunit))) (closure '() (fun #f "x" (aunit))) "fun test1")
   (check-equal? (eval-exp (fun "test-function" "x" (aunit))) (closure '() (fun "test-function" "x" (aunit))) "fun test2")
   
   ;; call tests
   (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1))) (int 8) "call test1")
   (check-equal? (eval-exp (call (closure '() (fun "add-function" "x" (add (var "x") (int 7)))) (int 1))) (int 8) "call test2")
   (check-equal? (eval-exp (call (closure '() (fun "add-function" "x" (add (var "x") (int 7)))) (int 1))) (int 8) "call test2")
   (check-exn exn:fail? (lambda () (eval-exp (call (aunit) (int 7)))) "call test3")
   
   ;; mlet tests
   (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet test1")
   (check-exn exn:fail? (lambda () (eval-exp (mlet (aunit) (int 1) (add (int 5) (var "x"))))) "mlet test2")
   
   ;; mpair test
   (check-equal? (eval-exp (apair (int 1) (int 1))) (apair (int 1) (int 1)) "apair test")

   ;; fst tests
   (check-equal? (eval-exp (fst (apair (int 1) (int 2)))) (int 1) "fst test1")
   (check-exn exn:fail? (lambda () (eval-exp (fst (aunit)))) "fst test2")
   
   ;; snd tests
   (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2) "snd test1")
   (check-exn exn:fail? (lambda () (eval-exp (snd (aunit)))) "snd test2")

   ;; aunit test
   (check-equal? (eval-exp (aunit)) (aunit) "snd test")
   
   ;; isaunit tests
   (check-equal? (eval-exp (isaunit (closure '() (fun #f "x" (aunit))))) (int 0) "isaunit test1")
   (check-equal? (eval-exp (isaunit (aunit))) (int 1) "isaunit test2")
   (check-equal? (eval-exp (isaunit (int 1))) (int 0) "isaunit test3")

   ;; closure test
   (check-equal? (eval-exp (closure '() (fun #f "x" (aunit)))) (closure '() (fun #f "x" (aunit))) "closure test")
   
   ;; ifaunit tests
   (check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test1")
   (check-equal? (eval-exp (ifaunit (aunit) (int 2) (int 3))) (int 2) "ifaunit test2")
   
   ;; mlet* tests
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "mlet* test1")
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10)) (cons "y" (int 10))) (add (var "x") (var "y")))) (int 20) "mlet* test2")
   (check-equal? (eval-exp (mlet* (list) (int 10))) (int 10) "mlet* test3")
   
   ;; ifeq tests
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test1")
   (check-equal? (eval-exp (ifeq (int 1) (int 1) (int 3) (int 4))) (int 3) "ifeq test2")
   
   ;; mupl-map tests
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit)))) (apair (int 8) (aunit)) "mupl-map test1")
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 2) (apair (int 1) (aunit))))) (apair (int 9) (apair (int 8) (aunit))) "mupl-map test2")
   
   ;; problems 1, 2, and 4 combined test
   (check-equal? (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                  (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))) (list (int 10) (int 11) (int 16)) "combined test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
