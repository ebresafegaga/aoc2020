#lang racket/base

(require (rename-in racket/match
                    [match-let let]
                    [match-let* let*])
         racket/list
         racket/file)

(define input-file "/home/ebresafegaga/repos/adventofcode/day3.txt")
(define input-test-file "/home/ebresafegaga/repos/adventofcode/day3-test.txt")

(define (lines file)
  (file->lines file))

(define (lines->grid lines)
  (grid (map (λ (x) (map char->cell (string->list x)))
             lines)))

(define (char->cell ch)
  (match ch
    [#\. (cell #f)]
    [#\# (cell #t)]))

(define (snoc xs x) (append xs (list x)))

(struct right (scalar) #:transparent)
(struct left (scalar) #:transparent)
(struct down (scalar) #:transparent)
(struct up (scalar) #:transparent)

(struct cell (tree?)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc cell-val output-port output-mode)
    (fprintf output-port (if (cell-tree? cell-val) "#" ".")))])
(struct grid (lines) #:transparent) ;; A grid is a row list, A row is a cell list
(struct zipper (focus before after) #:transparent) ;; A list Zipper
(struct zipper/state (before size)) ;; How many elements are before the focus
;; A grid zipper is also a list zipper
;; (struct gz (lz before after) #:transparent) ;; A grid zipper with a list Zipper as the focus

(define (zipper/size zp)
  (let ([(zipper _ before after) zp])
    (+ (length before)
       (length after)
       1)))

(define (get/state z)
  (match z
    [(zipper _ before _)
     (zipper/state (length before)
                   (zipper/size z))]))

(define (append/n lst n)
  (cond
    [(>= (length lst) n) lst]
    [else (append/n (append lst lst) n)]))

(define (replicate/state s lst)
  (match s  
    [(zipper/state before size)
     (let-values ([(b a) (split-at (append/n lst size) before)])
       (zipper (car a) b (cdr a)))])) 
 
(define (get-focus z)
  (match z
    [(zipper (zipper f _ _) _ _) f]
    [(zipper f _ _) f]))

(define (list->zipper lst)
  (match lst
    [(cons x xs) (zipper x null xs)]))

(define (zipper->list zip)
  (match zip
    [(zipper x xs ys)
     (append xs (list x) ys)]))

(define (grid->zipper grd)
  (match grd
    [(grid (cons line lines))
     (zipper (list->zipper line)
             null
             lines)]))

(define (gz-move-right zip)
  (match zip
    [(zipper lz _ _) 
     (struct-copy zipper
                  zip
                  [focus (move-right lz)])]))
(define (gz-move-left zip)
  (match zip
    [(zipper lz _ _)
     (struct-copy zipper
                  zip
                  [focus (move-left lz)])]))
(define (gz-move-down zip)
  (match zip
    [(zipper lz before (cons after afters))
     (let* ([st (get/state lz)]
            [af (replicate/state st after)])
       (zipper af
               (snoc before (zipper->list lz))
               afters))]))
(define (gz-move-up zip)
  (match zip
    [(zipper lz (list befores ... before) after)
     (zipper (list->zipper before)
             befores
             (cons (zipper->list lz) after))]))

(define (gz-move z direction)
  (match direction
    [(right 0) z]
    [(right x) (gz-move-right (gz-move z (right (sub1 x))))]
    [(left 0) z]
    [(left x) (gz-move-left (gz-move z (left (sub1 x))))]
    [(down 0) z]
    [(down x) (gz-move-down (gz-move z (down (sub1 x))))]
    [(up 0) z]
    [(up x) (gz-move-up (gz-move z (up (sub1 x))))]))

(define (gz-move/list zip #:by directions)
  (for/fold ([z zip])
            ([d directions])
    (gz-move z d)))

(define z (list->zipper (range 11)))

(define (on-last? z)
  (match z
    [(zipper _ _ '()) #t]
    [_ #f]))

(define (move-right zip)
  (match zip
    [(zipper f ys (cons x xs))
     (struct-copy zipper
                  zip
                  [focus x]  
                  [before (snoc ys f)]
                  [after xs])]
    [_ (move-right (double-lz zip))]))

(define (move-left zip)
  (match zip
    [(zipper f (list ys ... y) xs)
     (struct-copy zipper
                  zip
                  [focus y]  
                  [before ys]
                  [after (cons f xs)])]
    [_ zip]))

(define (move-right-by zip n)
  (for/fold ([z zip])
            ([i (range n)])
    (move-right z)))

(define (move-left-by zip n)
  (for/fold ([z zip])
            ([i (range n)])
    (move-left z)))

(define (move z direction #:by [scalar 1])
  (match direction
    ['left (move-left-by z scalar)]
    ['right (move-right-by z scalar)]))

(define (double-grid grd)
  (match grd
    [(grid lines)
     (grid (map (λ (line) (append line line))
                lines))]))

(define (double-lines lines)
  (map (λ (line) (string-append line line))
       lines))

(define (double-list lines)
  (map (λ (line) (append line line))
       lines))

(define (double-lz lz)
  (match lz
    [(and (zipper _ _ a)
          (var lz))
     (let* ([lz-list (zipper->list lz)]
            [l-double lz-list])
       (struct-copy zipper
                    lz
                    [after (append a l-double)]))]))

(define (double-gz gz)
  (match gz
    [(zipper lz before after)
     (let* ([before-double (double-list before)]
            [after-double (double-list after)])
       (zipper (double-lz lz)
               before-double
               after-double))]))

(define slope/original (list (right 3) (down 1)))
(define slope/a (list (right 1) (down 1)))
(define slope/b (list (right 5) (down 1)))
(define slope/c (list (right 7) (down 1)))
(define slope/d (list (right 1) (down 2)))

(define slopes
  (list slope/a
        slope/original
        slope/b
        slope/c
        slope/d))

(define (find z slope)
  (if (on-last? z)
      (list (get-focus z))
      (let ([z/1 (gz-move/list z #:by slope)])
        (cons (get-focus z) (find z/1 slope)))))

(define (find/n z slope)
  (length
   (filter cell-tree?
           (find z slope))))

(define (find/slopes z [sps slopes])
  (map (λ (slope) (find/n z slope))
       sps))

;; (define l (lines input-file))
(define l (lines input-test-file))
(define g (lines->grid l))
(define zp (grid->zipper g))

(define (find/zipper z slope)
  (if (on-last? z)
      (values (list (get-focus z)) z)
      (let*-values ([(z/1) (gz-move/list z #:by slope)]
                   [(cs z-final) (find/zipper z/1 slope)]
                   [(c) (get-focus z)])
        (values (cons c cs) z-final))))

(define (solve [z zp])
  (apply * (find/slopes z)))


