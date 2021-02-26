#lang typed/racket/base

(require racket/match
         racket/string
         racket/runtime-path
         racket/list
         racket/file)

(struct instruction ([rator : String]
                     [rand : Integer])
  #:transparent)
(struct (A) zipper ([before : (Listof (Pair A Boolean))]
                    [focus : (Pair A Boolean)]
                    [after : (Listof (Pair A Boolean))])
  #:transparent
  #:type-name Zipperof)
(struct machine ([instr : (Listof instruction)]))

(: remove-last (∀ (X) (-> (Listof X) (Listof X))))
(define (remove-last xs)
   (match xs
     [(cons _ '()) '()]
     [(cons x xs) (cons x (remove-last xs))]))

(: snoc (∀ (A) (-> (Listof A) A (Listof A))))
(define (snoc l a) (append l (list a)))

;; This semantics of this zipper operates under the assumption
;; that zippers are non-empty and the focus cannot go out
;; of the bounds of the list.
(: forward (∀ (A) (-> (Zipperof A) (Zipperof A))))
(define (forward zipp)
  (match zipp
    [(zipper before focus (cons a after))
     (match-let* ([(cons elem _) focus]
                  [focus (cons elem #t)])
         (zipper (snoc before focus) a after))]
    [_ zipp]))

(: back (∀ (A) (-> (Zipperof A) (Zipperof A))))
(define (back zipp)
  (match zipp
    [(zipper before focus after)
     (match-let* ([(cons elem _) focus] 
                  [a (last before)]
                  [before (remove-last before)]
                  [focus (cons elem #t)])
       (zipper before a (cons focus after)))]))

(: f-skip (∀ (A) (-> (Zipperof A) (Zipperof A))))
(define (f-skip zipp)
  (match zipp
    [(zipper before focus (cons a after))
     (zipper (snoc before focus) a after)]))

(: b-skip (∀ (A) (-> (Zipperof A) (Zipperof A))))
(define (b-skip zipp)
 (match zipp
   [(zipper before focus after) 
    (match-let* ([a (last before)]
                 [before (remove-last before)])
      (zipper before a (cons focus after)))]))

(: skipy (∀ (A) (-> (Zipperof A) (U 'front 'back) Integer (Zipperof A))))
(define (skipy zipp d by)
  (match* (by d)
    [{0 _} zipp]
    [{_ 'back}
     (skipy (b-skip zipp) 'back (sub1 by))]
    [{_ 'front}
     (skipy (f-skip zipp) 'front (sub1 by))]))

(: update (∀ (A) (-> A (Zipperof A) (Zipperof A))))
(define (update a zipp)
  (match zipp
    [(zipper before (cons _ v) after)
     (zipper before (cons a v) after)]))
(: zipper->list (∀ (A) (-> (Zipperof A) (Listof A))))
(define (zipper->list zipp)
  (match zipp
    [(zipper before focus after)
     (let ([all (append before (list focus) after)])
       (map (λ ([p : (Pair A Boolean)]) (car p)) all))]) )

(: move (∀ (A) (->* ((Zipperof A) (U 'front 'back))
                    (#:skip Boolean #:by Integer)
                    (Zipperof A))))
(define (move zipp direction #:skip [skip #f] #:by [by 1])
  (match by
    [0 zipp]
    [n
     (match* (direction skip)
       [{'front #t}
        (skipy (forward zipp) 'front (sub1 n))]
       [{'back #t}
        (skipy (back zipp) 'back (sub1 n))]
       [{'front _}
         (move (forward zipp) direction #:by (sub1 n))]
       [{'back _}
          (move (back zipp) direction #:by (sub1 n))])]))

(: visited? (∀ (A) (-> (Zipperof A) Boolean)))
(define (visited? zipp)
  (match zipp
    [(zipper _ (cons _ v) _) v]))
(: last? (∀ (A) (-> (Zipperof A) Boolean)))
(define (last? zipp)
  (match zipp
    [(zipper _ _ '()) #t]
    [_ #f]))

(: list->zipper (∀ (A) (-> (Listof A) (Zipperof A))))
(define (list->zipper l)
  (let ([l* (map (λ ([x : A]) (cons x #f)) l)])
    (match l*
      [(cons x xs) (zipper (list) x xs)])))

(: string->integer (-> String Integer))
(define (string->integer s)
  (assert (string->number s)
          exact-integer?))

(: parse (-> String instruction))
(define (parse s)
  (match-let ([(list op rand) (string-split s " ")])
    (instruction op (string->integer rand))))

(: interp (∀ (A) (-> instruction
                     (Zipperof A)
                     (Values (Zipperof A) Integer))))
(define (interp instr zipp)
  (match instr 
    [(instruction "nop" _) (values (move zipp 'front) 0)]
    [(instruction "acc" x) (values (move zipp 'front) x)]
    [(instruction "jmp" x)
     (let ([pos (positive? x)]
           [x* (abs x)]) 
       (if pos
           (values (move zipp 'front #:skip #t #:by x) 0)
           (values (move zipp 'back #:skip #t #:by x*) 0)))]))

(: exec (-> machine Integer))
(define (exec m)
  (let ([z (list->zipper (machine-instr m))])
    (let loop ([acc 0] [zipp z])
      (cond
        [(visited? zipp) acc]
        [else
         (match-let*-values ([((cons focus _)) (zipper-focus zipp)]
                             [(zipp* acc+) (interp focus zipp)])
            (loop (+ acc acc+) zipp*))]))))

(define-runtime-path file "day8.txt")
(define lines (file->lines file)) 
(define instr (map parse lines))
(define m (machine instr))
(exec m)

(: flip-op (-> Any instruction))
(define (flip-op instr)
  (match instr
    [(instruction "nop" scalar)
     (instruction "jmp" scalar)]
    [(instruction "jmp" scalar)
     (instruction "nop" scalar)]
    [(instruction _ _) instr])) 



(: neg-jmp? (-> instruction Boolean))
(define (neg-jmp? i)
  (match i
    [(instruction "jmp" x) (<= x 0)]
    [_ #f]))

(: jmp? (-> instruction Boolean))
(define (jmp? i)
  (match i
    [(instruction "jmp" _) #t]
    [_ #f]))

(: nop? (-> instruction Boolean))
(define (nop? i)
  (match i
    [(instruction "nop" _) #t]
    [_ #f]))

(define (can-flip [x : instruction])
  (or (nop? x)
      (jmp? x)))

(: loops? (-> (Listof instruction) Boolean))
(define (loops? i)
  (let ([z (list->zipper i)])
    (let loop ([zipp z])
      (cond
        [(last? zipp) #f]
        [(visited? zipp) #t]
        [else
         (match-let*-values ([((cons focus _)) (zipper-focus zipp)]
                             [(zipp* _) (interp focus zipp)])
                            (loop zipp*)) ]))))

(: fix3 (-> (Listof instruction) Integer))
(define (fix3 instr) 
  (let ([idx (map (λ ([a : instruction] [i : Integer]) (cons a i)) instr (range (length instr)))])
    (let loop ([idxs idx])
      (match idxs
        ['() 0]
        [(cons (cons ins index) idxs)
         (if (can-flip ins)
             (let ([new0 (list-update instr
                                      index
                                      (λ ([_ : instruction]) (flip-op ins)))])
               (if (loops? new0) (loop idxs) index))
             (loop idxs))])))) 

(: fix (-> (Listof instruction) (Listof instruction)))
(define fix 
  (λ (instr)
    (let ([index 
           (for/list : (Listof Integer)
             ([(elem index) (in-indexed instr)] 
              #:unless (loops? (list-update instr
                                      index
                                      flip-op)))
             index)])
      instr)))

(: exec-safe (-> machine Integer))
(define (exec-safe m)
   (let ([z (list->zipper (machine-instr m))])
    (let loop ([acc 0] [zipp z])
      (cond
        [(last? zipp)
         (match-let*-values ([((cons focus _)) (zipper-focus zipp)]
                             [(zipp* acc+) (interp focus zipp)])
            (+ acc acc+))]
        [else
         (match-let*-values ([((cons focus _)) (zipper-focus zipp)]
                             [(zipp* acc+) (interp focus zipp)])
            (loop (+ acc acc+) zipp*))]))))


(define ii
 (map parse
      (string-split "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6" "\n")))

;; 498 is gotten from (fix3 instr) -- It takes a while to compute
(define s (map (λ ([x : instruction] [i : Integer])
                 (if (= i 498)
                     (flip-op x)
                     x))
           instr (range (length instr))))
(exec-safe (machine s))

;; (fix ii)