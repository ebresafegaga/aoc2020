#lang racket/base

(require racket/match
         (for-syntax syntax/parse
                     racket/base))

(define-syntax cond/yes
  (syntax-rules (else/yes)
    [(_ (else/yes e1 e* ...)) (begin e1 e* ...)]))


(cond/yes
  [else/yes (define a 29) a])

(define-syntax match-list
  (λ (stx)
    (syntax-parse stx
      [(_ e:expr [() empty* ...+] [(a:id d:id) cons* ...+])
       #'(let ([v e])
           (cond
             [(null? v) empty* ...]
             [(pair? v) (let ([a (car v)] [d (cdr v)]) cons* ...)]
             [else (error 'match-list "Expected a pair or empty list")]))])))


(match-list (list 1 2 3)
  [() 0 0 0]
  [(a b) (+ a 5)])

(define-match-expander aba
  (syntax-rules ()
    [(aba a b) (list a b a)]))

(define-match-expander (number stx)
  (syntax-case stx (number)
    [(_ x) #'(? number? x)]))

(match (list 1 3 1)
  [(aba a b) (+ a b)])

(define-syntax (and2 stx)
  (syntax-case stx ()
    [(_ x y)
     #'(if x y #f)]))

(define-syntax my/when
  (λ (stx)
    (syntax-case stx ()
      [(_ e0 e1 e2 ...) (identifier? #'e0) #'(if e0 (begin e1 e2 ...))])))

(struct name (n))

(map syntax-e
     (syntax-e (syntax (list 1 2 3 4))))





