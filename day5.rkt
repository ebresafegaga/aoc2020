#lang typed/racket/base

(require racket/match
         racket/list
         racket/runtime-path
         racket/file)

(struct interval ([lower : Integer]
                  [upper : Integer]) #:transparent)
(define-type instruction (Listof Char))

(define intf/row (interval 0 127))
(define intf/col (interval 0 7))

(: split (-> interval (Pairof interval interval)))
(define (split intf)
  (let* ([u (interval-upper intf)]
         [l (interval-lower intf)]
         [d (- u l)]
         [haf (floor (/ d 2))]
         [a-l (+ l haf)]
         [a-u (add1 a-l)])
    (cons (interval l a-l)
          (interval a-u u))))

(: front (-> (Pairof interval interval) interval))
(define (front intfs)
  (car intfs))
(: back (-> (Pairof interval interval) interval))
(define (back intfs)
  (cdr intfs))

(: follow (-> instruction interval Integer))
(define (follow instr intf)
  (let ([intfs (split intf)])
    (match instr
      [(cons (or #\L #\F) rest) (follow rest (front intfs))]
      [(cons (or #\R #\B) rest) (follow rest (back intfs))]
      ;; at this point, the lower and upper bound of the
      ;; interval should be equal
      ['() (interval-lower intf)])))

(: parse/instr (-> (Listof Char)
                   (Values instruction instruction)))
(define (parse/instr lst)
  (let-values ([(rows cols) (split-at lst 7)])
    (values rows cols)))

(: follow/both (-> instruction
                   (Values Integer Integer)))
(define (follow/both instr)
  (let*-values ([(instr/r instr/c) (parse/instr instr)]
                [(row) (follow instr/r intf/row)]
                [(col) (follow instr/c intf/col)])
    (values row col)))

(: unique-id (-> instruction Integer))
(define (unique-id instr)
  (let-values ([(row col) (follow/both instr)])
    (+ (* row 8)
       col)))

(define-runtime-path file "day5.txt")

(define lines (λ () (map string->list (file->lines file))))

(: ans (->* () ((Listof instruction)) Integer))
(define (ans [lines (lines)])
  (apply max (map unique-id lines)))

(: all (∀ (X) (-> X X Integer (Listof (Listof X)))))
(define (all front back count)
  (cond
    [(<= count 0) (list (list))]
    [else
     (let* ([rest (all front back (sub1 count))]
            [left (map (λ ([xs : (Listof X)]) (cons front xs)) rest)]
            [right (map (λ ([xs : (Listof X)]) (cons back xs)) rest)])
       (append left right))]))

(: gen (-> (Listof instruction)))
(define gen
  (λ ()
    (let* ([first (all #\F #\B 7)]
           [last (all #\L #\R 3)]
           [joint (cartesian-product first last)])
      (map (λ ([xs : (Listof instruction)]) 
             (apply append xs)) joint))))

(: mt (Listof instruction))
(define mt empty)

(: missing (-> (Listof instruction)))
(define (missing) 
  (let ([given (lines)]
        [actual (gen)])
    (for/fold ([acc mt])
              ([seat actual])
      (let ([elem (member seat given)])
        (cond
          [(list? elem) acc]
          [else (cons seat acc)])))))

(: predicate (-> instruction Boolean))
(define (predicate instr)
  (let* ([id (unique-id instr)]
         [l (lines)]
         [l* (map unique-id l)]
         [id+1 (add1 id)]
         [id-1 (sub1 id)]
         [c (count (λ ([x : Integer])
                     (or (= x id+1)
                         (= x id-1))) l*)])
    (= c 2)))

(define m (missing))
;; (length l) == 1
(define l (filter predicate m))
(unique-id (car l))


