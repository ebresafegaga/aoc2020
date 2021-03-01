#lang racket/base

(require racket/require
         (multi-in racket (match file runtime-path list))
         megaparsack
         megaparsack/text
         data/applicative
         data/monad)

(struct bag (name) #:transparent)
(struct rule (bag items) #:transparent)

(define (can-hold-bag b rules r)
  (match r
    [(rule bag bags)
     (let ([bags* (map cdr bags)])
       (or (member b bags*)
           (ormap
            (λ (b*)
              (let ([rule (hash-ref rules b* (λ () (rule b* (list))))])
                (can-hold-bag b rules rule)))
            bags*)))]))

(define (count-bags b rules)
  (let ([b-rule (hash-ref rules b)])
    (match b-rule
      [(rule _ bags)
       (apply +
              (map
               (λ (b)
                 (let ([count (car b)]
                       [inner-bag (cdr b)])
                   (+ count (* count (count-bags inner-bag rules)))))
               bags))])))

(define space*/p (many/p space/p))

(define letters/p
  (do (text <- (many/p letter/p))
    (pure (list->string text))))

(define (bag/p str)
  (do (q <- letters/p)
    space*/p
    (n <- letters/p)
    space*/p
    (string/p str)
    space*/p
    (pure (bag (string-append q " " n)))))

(define scalar-bag/p
  (do (scalar <- integer/p)
    space*/p
    (bag <- (bag/p (if (scalar . > . 1) "bags" "bag")))
    (pure (cons scalar bag))))

(define sep/p
  (do (char/p #\,)
    space*/p))

(define no-bags/p
  (do (string/p "no other bags")
    (pure (list))))

(define rule/p
  (do (bag <- (bag/p "bags"))
    (string/p "contain")
    space*/p
    (items <- (or/p no-bags/p
                    (many/p scalar-bag/p #:sep sep/p)))
    (char/p #\.)
    (pure (rule bag items))))

(define (parse s)
  (parse-result! (parse-string rule/p s)))

(define (rules->hash rules)
  (define (f a s)
    (match a
      [(rule bag items) (hash-set s bag (rule bag items))]))
  (foldr f (hash) rules))

(define-runtime-path file "day7.txt")
(define rs (file->lines file))
(define h (map parse rs))
(define h* (rules->hash h))
(define b (bag "shiny gold"))
(count (λ (x) x)
       (map (λ (rule) (can-hold-bag b h* rule)) h))
(count-bags b h*)