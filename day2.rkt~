#lang racket/base

(require racket/file
         racket/match
         megaparsack
         megaparsack/text
         data/monad
         data/applicative)

(define input-file
  "/home/ebresafegaga/repos/adventofcode/day2.txt")

(struct range (upper lower) #:transparent)
(struct line (range char text) #:transparent)

(define (lines)
  (file->lines input-file))

(define (valid? input)
  (match-let* ([(line (range low up) char str) input]
               [chars (filter (λ (c) (eq? c char))
                              str)]
               [len (length chars)])
    (and (<= len up)
         (>= low len))))

(define (split line)
  line)

(define line/p
  (do [lower <- integer/p]
    (char/p #\-)
    [upper <- integer/p]
    space/p
    [char <- any-char/p]
    (char/p #\:)
    space/p
    [text <- (many/p any-char/p)]
    (pure (line (range upper lower)
                char
                text))))

(define (answer)
  (length
   (filter valid?
           (map (λ (line) (parse-result! (parse-string line/p
                                                       line)))
                (lines)))))






