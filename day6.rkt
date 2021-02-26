#lang racket/base

(require racket/match
         racket/file
         racket/function
         racket/runtime-path
         racket/set)

(define (parse l)
  (foldr (λ (a s)
             (match a
               ("" (cons (list) s))
               (_ (match s
                    ((cons x xs) (cons (cons a x) xs))
                    ('() (list (list a)))))))
           (list)
           l))

(define (count/any group)
  (let* ((g* (map string->list group))
         (all (apply append g*))
         (s (apply set all)))
    (set-count s)))

;; Can't believe Racket doesn't have this 
(define (reduce f xs)
  (match xs
    ((cons x '()) x)
    ((cons x xs) (f x (reduce f xs)))
    (_ (error "reduce: can't reduce an empty list"))))

(define (count/every group)  
  (let* ((g* (map (compose (curry apply set) string->list) group))
         (s (reduce set-intersect g*)))
    (set-count s)))

(define-runtime-path file "day6.txt")
(define l* (parse (file->lines file)))
(display "Any: ")
(apply + (map count/any l*))
(display "Every: ")
(apply + (map count/every l*))




