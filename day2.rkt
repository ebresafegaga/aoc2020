#lang racket/base

(require racket/file
         racket/match
         racket/function
         megaparsack
         megaparsack/text
         data/monad
         data/applicative)

(define ls-test
    '("1-3 a: abcde"
      "1-3 b: cdefg"
      "2-9 c: ccccccccc"))

(define input-file
  "/home/ebresafegaga/repos/adventofcode/day2.txt")

(struct range (upper lower) #:transparent)
(struct line (range char text) #:transparent)

(define (lines)
  (file->lines input-file))

(define (valid? pred input)
  (match-let* ([(line (range low high) char str) input]
               [chars (filter (λ (c) (eq? c char)) str)]
               [len (length chars)])
     (pred low high char str len)))

(define (1st-pred low high char str len)
  (and (<= len high)
       (>= len low)))

(define (2nd-pred low high char str len)
  (let* ([l (list-ref str (sub1 low))]
         [h (list-ref str (sub1 high))])
    (cond
      [(and (eq? l char)
            (not (eq? h char)))
       #t]
      [(and (eq? h char)
            (not (eq? l char)))
       #t]
      [else #f])))

(define (split line)
  line)

(define line/p
  (do (lower <- integer/p)
    (char/p #\-)
    (upper <- integer/p)
    space/p
    (char <- any-char/p)
    (char/p #\:)
    space/p
    (text <- (many/p any-char/p))
    eof/p
    (pure (line (range lower upper)
                char
                text))))

(define (answer/1 ls)
  (length
   (filter (curry valid? 1st-pred)
           (map (λ (line) (parse-result! (parse-string line/p
                                                       line)))
                ls))))

(define (answer/2 ls)
  (length
   (filter (curry valid? 2nd-pred)
           (map (λ (line) (parse-result! (parse-string line/p
                                                       line)))
                ls))))

(define (string->range line)
  (parse-result!
   (parse-string line/p
                 line)))

(printf "Part 1: ~a \n" (answer/1 (lines)))
(printf "Part 2: ~a \n" (answer/2 (lines)))
