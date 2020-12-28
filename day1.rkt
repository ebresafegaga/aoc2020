#lang racket 

(define input-file
  "/home/ebresafegaga/repos/adventofcode/day1.txt")

(define test-values
  '(1721
    979
    366
    299
    675
    1456))

(define (answer/list numbers)
  (call/cc
   (λ (k)
     (for ([n numbers])
       (for ([x (remove n numbers)])
         (if (= 2020 (+ n x))
             (k (* n x))
             (void)))))))

(define (lines file)
  (call-with-input-file file
    (λ (port)
      (for/list ([line (in-lines port)])
        (string->number line)))))

(define (answer/file file)
  (answer/list (lines file)))

(define (main)
  (answer/file input-file))



