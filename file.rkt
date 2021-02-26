#lang slideshow/widescreen

(require racket/cmdline
         slideshow/base
         pict)

(provide my-+
         my-*)

(define f assv)

(slide #:title "Big fish"
       (standard-fish 200 100 #:color "blue")
       (rectangle 10 30)
       (cloud 111 121))

(define (my-+ a b)
  (if (zero? a)
      b
      (my-+ (sub1 a) (add1 b))))
 
(define (my-* a b)
  (if (zero? a)
      b
      (my-* (sub1 a) (my-+ b b))))

(define f/1 open-output-string)

(define my-param
  (let ([value #f])
    (case-lambda
      [() value]
      [(new) (set! value new)])))



