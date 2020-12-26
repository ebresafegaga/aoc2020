#lang racket/base

(require racket/match
         racket/file
         rackunit)

(define input-file
  "/home/ebresafegaga/repos/adventofcode/day1.txt")

(define test-values
  '(1721
    979
    366
    299
    675
    1456))

(define (answer/list/2 numbers)
  (call/cc
   (λ (k)
     (for ([n numbers])
       (for ([x (remove n numbers)])
         (if (= 2020 (+ n x))
             (k (* n x))
             (void)))))))

(define (answer/list/3 numbers)
  (call/cc
   (λ (k)
     (for ([n numbers])
       (for ([x (remove n numbers)])
         (for ([y (remove n (remove x numbers))])
           (if (eq? 2020 (+ n x y))
               (k (* n x y))
               (void))))))))


(define (lines file)
  (call-with-input-file file
    (λ (port)
      (for/list ([line (in-lines port)])
        (string->number line)))))

(define (lines/2 file)
  (map string->number
       (file->lines file)))

(define (answer/file f file)
  (f (lines file)))

(define (main number)
  (match number
    [2 (answer/file answer/list/2 input-file)]
    [3 (answer/file answer/list/3 input-file)]
    [_ 0]))

