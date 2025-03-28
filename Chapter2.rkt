#lang sicp

;;; CODE FROM OTHER CHAPTERS OF STRUCTURE AND INTERPRETATION OF ;;; COMPUTER PROGRAMS NEEDED BY CHAPTER 2 ;;;from chapter 1
(define (square x) (* x x)) ;;;from section 1.2.5, for Section 2.1.1
(define (gcd a b) (if (= b 0) a (gcd b (remainder a b)))) ;;;from section 1.2.2, for Section 2.2.3
(define (fib n) (cond ((= n 0) 0) ((= n 1) 1) (else (+ (fib (- n 1)) (fib (- n 2)))))) ;;; ***not in book, but needed for code before quote is introduced***
(define nil '()) ;;;----------- ;;;from section 3.3.3 for section 2.4.3 ;;; to support operation/type table for data-directed dispatch
(define (assoc key records) (cond ((null? records) false) ((equal? key (caar records)) (car records)) (else (assoc key (cdr records))))) (define (make-table) (let ((local-table (list '*table*))) (define (lookup key-1 key-2) (let ((subtable (assoc key-1 (cdr local-table)))) (if subtable (let ((record (assoc key-2 (cdr subtable)))) (if record (cdr record) false)) false))) (define (insert! key-1 key-2 value) (let ((subtable (assoc key-1 (cdr local-table)))) (if subtable (let ((record (assoc key-2 (cdr subtable)))) (if record (set-cdr! record value) (set-cdr! subtable (cons (cons key-2 value) (cdr subtable))))) (set-cdr! local-table (cons (list key-1 (cons key-2 value)) (cdr local-table))))) 'ok) (define (dispatch m) (cond ((eq? m 'lookup-proc) lookup) ((eq? m 'insert-proc!) insert!) (else (error "Unknown operation -- TABLE" m)))) dispatch)) (define operation-table (make-table)) (define get (operation-table 'lookup-proc)) (define put (operation-table 'insert-proc!)) ;;;-----------

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (and (= n (smallest-divisor n))
       (not (= n 1)))
  )
;;; END imported helper code

(define x (cons 1 2))
(define (average x y) (/ (+ x y) 2))

(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

; 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((or (and (< n 0) (< d 0)) (and (> n 0) (< d 0)))
           (cons (/ (- 0 n) g) (/ (- 0 d) g)))
          (else (cons (/ n g) (/ d g))))))

;(print-rat (make-rat 3 6))
;(print-rat (make-rat -3 6))
;(print-rat (make-rat 3 -6))
;(print-rat (make-rat -3 -6))

;2.2
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (make-segment start end)
  (cons start end))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))
(define (midpoint-segment segment)
  (make-point (average (x-point (start-segment segment))
                       (x-point (end-segment segment)))
              (average (y-point (start-segment segment))
                       (y-point (end-segment segment)))))

;(print-point (midpoint-segment (make-segment (make-point 3 2) (make-point 5 5))))

;2.3
; First implementation: Single segment spanning bottom left to top right corner, type 1
(define (make-rectangle1 segment) (cons 1 segment))

; Second implementation: Base point, width, height, type 2
(define (dimensions width height)
  (cons width height))
(define (width dimensions) (car dimensions))
(define (height dimensions) (cdr dimensions))
(define (rectangle2 base-point dimensions)
  (cons base-point dimensions))
(define (base-point rectangle) (car rectangle))
(define (dimensions-rect rectangle) (cdr rectangle))
(define (make-rectangle2 base-point dimensions)
  (cons 2 (rectangle2 base-point dimensions)))

; Selectors: Check which implementation and branch accoridngly
(define (get-type rectangle) (car rectangle))
(define (get-rect rectangle) (cdr rectangle))
(define (width-rectangle rectangle)
  (define type (get-type rectangle))
  (define rect (get-rect rectangle))
  (cond ((= type 1) (- (x-point (end-segment rect))
                       (x-point (start-segment rect))))
        ((= type 2) (width (dimensions-rect rect)))))

(define (height-rectangle rectangle)
  (define type (get-type rectangle))
  (define rect (get-rect rectangle))
  (cond ((= type 1) (- (y-point (end-segment rect))
                       (y-point (start-segment rect))))
        ((= type 2) (height (dimensions-rect rect)))))

(define (rectangle-area rectangle)
  (* (width-rectangle rectangle) (height-rectangle rectangle)))
(define (rectangle-perimeter rectangle)
  (* 2 (+ (width-rectangle rectangle) (height-rectangle rectangle))))

(define r1 (make-rectangle1 (make-segment (make-point 1 1) (make-point 5 6))))
(define r2 (make-rectangle2 (make-point 1 1) (dimensions 5 6)))
;(rectangle-area r1)
;(rectangle-perimeter r1)
;(rectangle-area r2)
;(rectangle-perimeter r2)

;2.4
; The equivalent definition is
; (define (cdr z)
;    (z (lambda (p q) q)))
; i.e. cons produces a function that takes a function as an argument and applies that argument onto the pair
; and for cdr, we provide a lambda function that takes in two arguments and returns the second

;2.5
(define (pow base exp)
  (if (= 0 exp)
      1
      (* base (pow base (- exp 1)))))
(define (cons-numeric a b) (* (pow 2 a) (pow 3 b)))
(define (get-exponent n base)
  (if (= (modulo n base) 0)
      (+ 1 (get-exponent (/ n base) base))
      0
      ))

(define (car-numeric n) (get-exponent n 2))
(define (cdr-numeric n) (get-exponent n 3))

(define p (cons-numeric 5 6))

;(car-numeric p)
;(cdr-numeric p)

;2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;The church numeral n represents the action of composing a function n times
;Therefore, if the church numeral n is applied to the increment function, the
; result is a function that increments n times. Passing zero to this resulting function
; yields the 'traditional' integer n.
; ((zero inc) 0) ; composes the 'inc' function 0 times -> we are left with the identity function
; (((add-1 zero) inc) 0) ; simplifies to (inc 0)
; In other words, the church numeral n is the exact same thing as (repeated n), which we previously saw

(define one (lambda (f) (lambda (x) (f x))))
;((one inc) 0)

(define two (lambda (f) (lambda (x) (f (f x)))))
;((two (two inc)) 0)

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(define (mult a b)
  (lambda (f) (lambda (x)
                ((a (b f)) x))))
;(((mult two two) inc) 0)
;(((add two one) inc) 0)

;2.7
(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (<= 0 (upper-bound y)))
      (display "error dividing by zero")
      (mul-interval x
                    (make-interval
                     (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))

;2.8
(define (sub-interval x y)
  (add-interval x
                (make-interval
                 (- 0 (upper-bound y))
                 (- 0 (lower-bound y)))))

;(define unit (make-interval 1/2 1))
;(define u2 (make-interval -2 4))

;(add-interval unit u2)
;(sub-interval unit u2)

;2.9
; Width of add-interval: ((upper x) + (upper y) - ((lower x) + (lower y))) / 2
; = ((upper x) - (lower x)) / 2 + ((upper y) - (lower y)) / 2
; = width x + width y
; Width of sub-interval: (((upper x) - (lower y)) - ((lower x) - (upper y))) / 2
; = ((upper x) - (lower x)) / 2 + ((upper y) - (lower y)) / 2
; = width x + width y

; (mul-interval (0 0) (0 1)) = (0 0), with width 0
; but (mul-interval (1 1) (0 1)) = (0 1), with width 1, yet the widths are the same
; (div-interval (0 0) (1 2)) = (0 0), width 0
; but (div-interval (1 1) (1 2)) = (1/2 1) with width 1

;2.10
;(div-interval u2 unit)

;2.11
(define (span-zero? x)
  (and (< (lower-bound x) 0) (< 0 (upper-bound x))))
(define (positive-interval? x)
  (< 0 (lower-bound x)))
(define (mul-interval2 x y)
  (cond ((and (span-zero? x) (span-zero? y))
         (make-interval (min (* (upper-bound x)
                                (lower-bound y))
                             (* (lower-bound x)
                                (upper-bound y)))
                        (max (* (lower-bound x)
                                (lower-bound y))
                             (* (upper-bound x)
                                (upper-bound y)))))
        ((and (span-zero? x) (positive-interval? y))
         (make-interval (* (lower-bound x) (upper-bound y))
                        (* (upper-bound x) ((upper-bound y)))))
        ((and (span-zero? x) (not (positive-interval? y)))
         (make-interval (* (upper-bound x) (lower-bound y))
                        (* (lower-bound x) (lower-bound y))))
        ((and (positive-interval? x) (span-zero? y))
         (make-interval (* (upper-bound x) (lower-bound y))
                        (* (upper-bound x) (upper-bound y))))
        ((and (positive-interval? x) (positive-interval? y))
         (make-interval (* (lower-bound x) (lower-bound y))
                        (* (upper-bound x) (upper-bound y))))
        ((and (positive-interval? x) (not (positive-interval? y)))
         (make-interval (* (upper-bound x) (lower-bound y))
                        (* (lower-bound x) (upper-bound y))))
        ((and (not (positive-interval? x)) (span-zero? y))
         (make-interval (* (lower-bound x) (upper-bound y))
                        (* (lower-bound x) (lower-bound y))))
        ((and (not (positive-interval? x)) (positive-interval? y))
         (make-interval (* (lower-bound x) (upper-bound y))
                        (* (upper-bound x) (lower-bound y))))
        ((and (not (positive-interval? x)) (not (positive-interval? y)))
         (make-interval (* (upper-bound x) (upper-bound y))
                        (* (lower-bound x) (lower-bound y))))))

;2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(define (width-i i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

(define (make-center-percent center percent)
  (define width (* center (/ percent 100)))
  (make-interval (- center width)
                 (+ center width)))

(define (percent interval)
  (* 100 (/ (width-i interval) (center interval))))

;(percent (make-center-percent 100 20))
;2.13
; If interval a has percentage tolerance x%, and interval b has percentage tolerance y%,
; then interval a * b has percentage tolerance (1 - (1 + a) * (1 + b))

;2.14
(define (par1 r1 r2)
  (div-interval
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one
     (add-interval
      (div-interval one r1)
      (div-interval one r2)))))

(define res1 (make-interval 95 105))
(define res2 (make-interval 49 51))

;(percent (par1 res1 res1))
;(percent (par1 res2 res2))

;(percent (par2 res1 res1))
;(percent (par2 res2 res2))
; par1 reports a ~3x increase in percentage tolerance compared to par2!
;2.15
; Yes, each time an interval with uncertainty is introduced into the computation, more uncertainty is added.
; In par1, we introduce R1 and R2 twice each, whereas par2 introduces them once each only.

;2.16
; This task is impossible. To achieve this, we would need to have a program that is able to reduce ANY
; combination of arithmetic operations involving multiple instances of a free variable into a representation
; where the free variable appears only once.

;2.17, assume l is non-empty
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))
;(last-pair (list 23 72 149 34))

;2.18: THE classic
(define (reverse l)
  (define (reverse-helper li)
    (if (null? (cdr li))
        (car li)
        (reverse-helper (cons (cons (cadr li) (car li))
                              (cddr li)))))
  (reverse-helper (cons (cons (car l) nil)
                        (cdr l))))
;(reverse (list 1 4 9 16 25 36))

(define (reverse-a l)
  (if (null? l)
      l
      (append (reverse-a (cdr l)) (list (car l)))))

;(reverse-a (list 1 4 9 16 25 36))
(define (first-n n)
  (define (first-n-iter current count)
    (if (= n count)
        current
        (first-n-iter (append current
                              (list count))
                      (+ count 1))))
  (first-n-iter '() 0))

;(define l (first-n 10000))

(define (timed-test procedure input)
  (newline)
  (define start-time (runtime))
  (procedure input)
  (display (- (runtime) start-time)))

;(timed-test reverse l) ; 28 for 10000, 770 for 50000
;(timed-test reverse-a l) ; 141464 for 5633747 for 50000
; Reversing with appending is many orders of magnitude worse than mine.
; Time complexity of reverse-a is O(n^2), whereas reverse is O(n)

;2.19
(define (first-denomination coins) (car coins))
(define (except-first-denomination coins) (cdr coins))
(define (no-more? coins) (null? coins))

(define (cc amount coin-values)
  (cond ((= amount 0)
         1)
        ((or (< amount 0)
             (no-more? coin-values))
         0)
        (else
         (+ (cc
             amount
             (except-first-denomination
              coin-values))
            (cc
             (- amount
                (first-denomination
                 coin-values))
             coin-values)))))

(define us-coins
  (list 1 50 10 5 25))

(define uk-coins
  (list 100 50 20 10 5 2 1 0.5))
;(display 5)
;(cc 100 us-coins)
; Order doesn't matter. The breakdown 100 = 50 + 25 + 10 + 5 + 5 + 5 will be counted
; under the reordering (1 50 10 5 25) as 100 = 50 + 10 + 5 + 5 + 5 + 25

;2.20
(define (same-parity num . li)
  (define (same-parity-helper n l)
    (if (null? l)
        l
        (if (= 0 (remainder (- n (car l)) 2))
            (cons (car l)
                  (same-parity-helper n (cdr l)))
            (same-parity-helper n (cdr l)))))
  (cons num (same-parity-helper num li)))

;(same-parity 1 2 3 4 5 6 7)
;(same-parity 2 3 4 5 6 7)
#|
(define (map proc l)
  (if (null? l)
      nil
      (cons (proc (car l))
            (map proc (cdr l)))))
|#
;(map abs (list -10 2.5 -11.6 17))
;(map (lambda (x) (* x x)) (list 1 2 3 4))
;2.21
(define (square-list1 items)
  (if (null? items)
      nil
      (cons (* (car items) (car items))
            (square-list1 (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

;(square-list1 (list 1 2 3 4))
;(square-list2 (list 1 2 3 4))

;2.22
; The first produces a list in backwards order because elements are taken from the front
; of the original list, and put at the BACK of the newly created list
; The second procedure has the elements in the 'right order', but the structure of the output
; does not correspond to lists as we understand them, it has the data in the cdr and the remaining list
; in the car

;2.23
(define (for-each proc items)
  (if (null? items)
      #t
      (begin (proc (car items))
             (for-each proc (cdr items)))))

;(for-each
; (lambda (x) (newline) (display x))
; (list 57 321 88 85))

;2.24
; (1 (2 (3 4)))
;(list 1 (list 2 (list 3 4)))

;2.25
;(car (cdaddr (list 1 3 (list 5 7) 9)))
;(caar (list (list 7)))
;(cadadr (cadadr (cadadr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))

;2.26
(define x! (list 1 2 3))
(define y! (list 4 5 6))
;(append x! y!) ; (1 2 3 4 5 6)
;(cons x! y!) ; ((1 2 3) 4 5 6)
;(list x! y!) ; ((1 2 3) (4 5 6))

;2.27: TODO: rewrite original reverse to be cleaner, try to remove helper function
(define x!!
  (list (list 1 2) (list 3 4)))

(define (deep-reverse l)
  (define (reverse-helper li)
    (if (null? li)
        nil
        (reverse-helper (cons (cons (cadr li) (car li))
                              (cddr li)))))
  (reverse-helper (cons (cons (car l) nil)
                        (cdr l))))

;x!!

;2.28
(define (fringe tree)
  (cond ((null? tree) nil)
        ((not (pair? (car tree)))
         (append (list (car tree))
                 (fringe (cdr tree))))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

(define x!!!
  (list (list 1 2) (list 3 4)))

;(fringe x!!!)
;(fringe (list x!!! x!!!))

;2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;1.
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

;2.
(define (get-structure branch)
  (cadr branch))
(define (total-weight mobile)
  (if (number? mobile)
      mobile
      (+ (total-weight (get-structure (left-branch mobile)))
         (total-weight (get-structure (right-branch mobile))))))

(define b1 (make-branch 2 5))
(define b2 (make-branch 1 10))
;(total-weight (get-structure b1))
(define unbalanced (make-mobile (make-branch 1 (make-mobile b1 b2)) b1))
;(total-weight unbalanced)
;3.
(define (torque branch)
  (* (car branch) (total-weight (get-structure branch))))
(define (balanced? mobile)
  (cond ((number? mobile) #t)
        ((= (torque (left-branch mobile))
            (torque (right-branch mobile)))
         (and (balanced? (get-structure (left-branch mobile)))
              (balanced? (get-structure (right-branch mobile)))))
        (else #f)))
;(balanced? unbalanced)
;(balanced? (make-mobile b1 b2))
;4. I just need to change cadr to become cdr in my selectors

;2.30
(define tree-instance (list 1
                            (list 2 (list 3 4) 5)
                            (list 6 7)))

(define (square-tree-direct tree)
  (if (null? tree)
      nil
      (if (pair? tree)
          (cons (square-tree-direct (car tree))
                (square-tree-direct (cdr tree)))
          (* tree tree))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (number? sub-tree)
             (* sub-tree sub-tree)
             (square-tree-map sub-tree)))
       tree))

;(square-tree-direct tree-instance)
;(square-tree-map tree-instance)

;2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (number? sub-tree)
             (proc sub-tree)
             (tree-map proc sub-tree)))
       tree))

;(tree-map square tree-instance)

;2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset)
                            (append (list (car s)) subset)) rest)))))

; rest is the set of all subsets not containing the first element of s
; since we can split the subsets of s into exactly two equal halves: those containing
; the first element, and those that do not, we can produce the former from the latter
; by prepending the first element of s to each subset

;(subsets (list 1 2 3))

;2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

;(accumulate * 1 (list 1 2 3 4 5))
(define (map-accum p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil sequence))

;(map-accum square (list 1 2 3 4 5))

(define (append-accum seq1 seq2)
  (accumulate cons seq2 seq1))

;(append-accum (list 1 2 3 4 5) (list 6 7 8 9))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;(length (append-accum (list 1 2 3 4 5) (list 6 7 8 9)))

;2.34
(define
  (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))

;(horner-eval 2 (list 1 3 0 5 0 1))

(define tree-x (cons (list 1 2) (list 3 4)))
;tree-x
;2.35
(define (get-leaves tree)
  (cond ((null? tree) nil)
        ((number? tree) (list tree))
        (else (append (get-leaves (car tree))
                      (get-leaves (cdr tree))))))
(define (count-leaves t)
  (accumulate + 0 (map (lambda (tree)
                         (if (number? tree)
                             1
                             (count-leaves tree))) t)
              ))
;(count-leaves tree-x)

;2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (sequence) (car sequence)) seqs))
            (accumulate-n op init (map (lambda (sequence) (cdr sequence)) seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
;(accumulate-n + 0 s)

;2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;(dot-product (list 1 2 3) (list 3 3 3))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define I3 (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(define A3 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
;(matrix-*-vector I3 (list 1 2 3))

(define (transpose mat)
  (accumulate-n cons nil mat))

;(transpose I3)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

;(matrix-*-matrix A3 A3)

;2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;(accumulate / 1 (list 1 2 3)) ; 3/2
;(fold-left  / 1 (list 1 2 3)) ; 1/6
;(accumulate list nil (list 1 2 3)) ; (1 (2 (3 ())))
;(fold-left  list nil (list 1 2 3)) ; (((() 1) 2) 3)
;(accumulate (lambda (x y) x) 0 (list 1 2 3)) ; 1
;(fold-left (lambda (x y) x) 0 (list 1 2 3)) ; 0

;(accumulate + 1 (list 1 2 3))
;(fold-left + 1 (list 1 2 3))

; (accumulate * init (x y z)) = (x * (y * (z * init)))
; but (fold-left * init (x y z)) = (((init * x) * y) * z)
; so fold-left and fold-right are equal when the operation is associative AND commutative, OR
; if the operation is ONLY associative AND the initial element is the identity element

;2.39
(define (fold-right op initial l)
  (if (null? l)
      initial
      (op (car l)
          (fold-right op initial (cdr l)))))
(define (reverse-fr sequence)
  (fold-right
   (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-fl sequence)
  (fold-left
   (lambda (x y) (append (list y) x)) nil sequence))

;(reverse-fr (list 1 2 3 4))
;(reverse-fl (list 1 2 3 4))

(define (enumerate-interval start end)
  (if (> start end)
      nil
      (cons start (enumerate-interval (+ start 1) end))))

;(enumerate-interval 1 5)

(define (filter pred seq)
  (if (null? seq)
      nil
      (if (pred (car seq))
          (cons (car seq) (filter pred (cdr seq)))
          (filter pred (cdr seq)))))

;(filter (lambda (x) (= 0 (remainder x 2))) (enumerate-interval 1 10))
(define (remove item sequence)
  (filter (lambda (x) (not (= item x))) sequence))

;(remove 5 (remove 6 (enumerate-interval 1 10)))

; flatmap: if each element in the sequence produces multiple elements, and we want to combine
; all such created elements, then we have to flatmap
; this avoids (((1 2) (1 3)) ((2 3) (2 4))), and gives ((1 2) (1 3) (2 3) (2 4))
(define (flatmap procedure sequence)
  (accumulate append nil (map procedure sequence)))

;2.40
(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;(unique-pairs 5)

(define (prime-sum-pairs n)
  (filter (lambda (pair) (prime? (+ (car pair) (cadr pair))))
          (unique-pairs n)))

;(prime-sum-pairs 6)

;2.41
(define (unique-triples n)
  (flatmap (lambda (i) (map (lambda (pair) (append (list i) pair))
                            (unique-pairs (- i 1))))
           (enumerate-interval 3 n)))
;(unique-triples 5)
(define (triple-sum max target)
  (filter (lambda (triple) (= (+ (car triple)
                                 (cadr triple)
                                 (caddr triple))
                              target))
          (unique-triples max)))

;(triple-sum 10 10)

;2.42
(define empty-board nil) ; board representation is ((row, col))
(define (make-pos row col) (cons row col))
(define (row position) (car position))
(define (col position) (cdr position))

(define (attack? p1 p2)
  (cond ((and (= (row p1) (row p2)) (= (col p1) (col p2))) #f)
        ((= (row p1) (row p2)) #t)
        ((= (col p1) (col p2)) #t)
        ((= (abs (- (row p1)
                    (row p2)))
            (abs (- (col p1)
                    (col p2)))) #t)
        (else #f)))

(define (safe? k positions)
  (define pos (car (filter (lambda (position) (= k (col position))) positions)))
  (fold-left (lambda (accum other) (and accum (not (attack? pos other)))) 
             #t
             positions))

;(safe? 5 (list (make-pos 1 5) (make-pos 5 1) (make-pos 2 2) (make-pos 1 3)))
(define (adjoin-position row col positions)
  (cons (make-pos row col) positions))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate-interval 
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;(map (lambda (i) (length (queens i))) (enumerate-interval 1 10))
; amazing!

;2.43
; The original procedure computes the subproblem (queen-cols (- k 1)) exactly once for each k in the
; range (1,...,board-size), but Louis' program calls queen-cols(- k 1) eight times for each k!
; a first approximation then says that his program would take 8^7 times as long to computer 8-queens

;2.44
