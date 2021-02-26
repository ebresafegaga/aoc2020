#lang typed/racket/base

(require racket/file
         racket/string
         racket/list
         racket/function
         racket/match)

(define file "/home/ebresafegaga/repos/adventofcode/day4.txt")

(define test-data
  "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007")

(: string->integer (-> String Integer))
(define (string->integer s)
  (assert (string->number s)
          exact-integer?))

(: integer? (-> String Boolean))
(define (integer? s)
   (cond
     [(exact-integer? (string->number s)) #t]
     [else #f]))

(: and/p (-> (-> String Boolean) *
             (-> String Boolean)))
(define (and/p . ps)
  (λ (s)
    (andmap (λ ([f : (-> String Boolean)]) (f s))
            ps)))

(: or/p (-> (-> String Boolean) *
            (-> String Boolean)))
(define (or/p . ps)
  (λ (s)
    (ormap (λ ([f : (-> String Boolean)]) (f s))
            ps)))

(: between/p (-> Integer Integer (-> String Boolean)))
(define (between/p lower upper)
  (λ (s)
    (if (integer? s)
        (<= lower (string->integer s) upper)
        #f)))

(: equal/p (-> String (-> String Boolean)))
(define ((equal/p x) s) (string=? x s))

(: hcl/p (-> String Boolean))
(define (hcl/p s)
  (match-let*-values ([(l) (string->list s)]
                      [((list hash) rest) (split-at l 1)])
    (and (char=? hash #\#)
         (and (andmap
               (λ ([c : Char])
                 (or (<= 97 (char->integer c) 102)
                     (<= 48 (char->integer c) 57)))
               rest)
              (= 6 (length rest))))))

(: believe-me (-> (Listof Any) (Listof Char)))
(define (believe-me xs)
  (match xs
    [(cons x xs)
     (cond
       [(char? x) (cons x (believe-me xs))]
       [else (error "I don't believe you o!")])]
    ['() '()]))

(: hgt/p (-> String Boolean))
(define (hgt/p s)
  (let ([l (string->list s)])
    (match l
      [(list num ... #\c #\m)
       (let ([s (list->string (believe-me num))]
             [f (between/p 150 193)])
         (f s))]
      [(list num ... #\i #\n)
       (let ([s (list->string (believe-me num))]
             [f (between/p 59 76)])
         (f s))]
      [_ #f])))

(define colors
  (list "amb" "blu"
        "brn" "gry"
        "grn" "hzl" "oth"))

(: predicates (Listof (Pair String (-> String Boolean))))
(define predicates
  (list (cons "byr" (between/p 1920 2002))
        (cons "iyr" (between/p 2010 2020))
        (cons "eyr" (between/p 2020 2030))
        (cons "ecl" (apply or/p (map equal/p colors)))
        (cons "cid" (const #t))
        (cons "pid" (and/p integer?
                           (λ ([s : String])
                             (= 9 (string-length s)))))
        (cons "hcl" hcl/p)
        (cons "hgt" hgt/p)))

(: satisfy/p (->* ((Listof (Pair String String)))
                  ((Listof (Pair String (-> String Boolean))))
                  Boolean))
(define (satisfy/p xs [ps predicates])
  (andmap
   (λ ([s : (Pair String String)])
     (let ([fs (assoc (car s) ps)])
       (cond 
         [(cons? fs) ((cdr fs) (cdr s))]
         [else #t])))
   xs))

(define normal-req
  (list "ecl" "pid"
        "eyr" "hcl"
        "byr" "iyr"
        "cid" "hgt"))
(define north-req
  (remove "cid"
          normal-req))

(: satisfy (-> (Listof String) (Listof String) Boolean))
(define (satisfy req lst)
  (andmap (λ ([x : String]) (if (member x lst) #t #f))
          req))

(: sat (-> (Listof String) Boolean))
(define (sat l)
  (or (satisfy north-req l)
      (satisfy normal-req l)))

(define (lines) (file->string file))

(: split (->* () (String) (Listof String)))
(define (split [l (lines)]) (string-split l "\n") )

(: parse (->* () ((Listof String)) (Listof String)))
(define (parse [l (split)])
  (foldr
   (λ ([a : String] [s : (Listof String)])
     (match a
       ["" (cons "" s)]
       [_ (match s
            [(cons x xs) (cons (string-append a " " x) xs)]
            ['() (list a)])]))
   '()
   l))

(: valid? (-> String Boolean))
(define (valid? s)
  (let* ([fields (string-split s " ")]
         [m (map (λ ([field : String])
                   (car (string-split field ":")))
                 fields)])
    (sat m)))

(: more-valid? (-> String Boolean))
(define (more-valid? s)
  (let* ([fields (string-split s " ")]
         [m (map
             (λ ([field : String])
               (match-let ([(list x y) (string-split field ":")])
                 (cons x y)))
             fields)])
    (satisfy/p m)))

(: answer (->* () ((Listof String)) (Listof String)))
(define (answer [l (parse)])
  (filter (λ ([x : String])
            (and (valid? x)
                 (more-valid? x)))
          l))