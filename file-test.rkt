#lang racket/base

(require rackunit
         rackunit/text-ui
         "file.rkt")

(define file-tests
  (test-suite
   "Tests for file.rkt"
   (check-equal? (my-+ 1 1) 2 "Simple Addition")
   (check-equal? (my-* 1 2) 2 "Simple Multiplication")
   
   (test-case
    "List has four elements and is even"
    (let ([lst (list 2 4 6 9)])
      (check = (length lst) 4)
      (for-each
       (λ (elt)
         (check-pred even? elt))
       lst)))))

(run-tests file-tests)











