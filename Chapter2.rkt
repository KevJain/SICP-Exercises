#lang sicp

;;; CODE FROM OTHER CHAPTERS OF STRUCTURE AND INTERPRETATION OF ;;; COMPUTER PROGRAMS NEEDED BY CHAPTER 2 ;;;from chapter 1
(define (square x) (* x x)) ;;;from section 1.2.5, for Section 2.1.1
(define (gcd a b) (if (= b 0) a (gcd b (remainder a b)))) ;;;from section 1.2.2, for Section 2.2.3
(define (fib n) (cond ((= n 0) 0) ((= n 1) 1) (else (+ (fib (- n 1)) (fib (- n 2)))))) ;;; ***not in book, but needed for code before quote is introduced***
(define nil '()) ;;;----------- ;;;from section 3.3.3 for section 2.4.3 ;;; to support operation/type table for data-directed dispatch
(define (assoc key records) (cond ((null? records) false) ((equal? key (caar records)) (car records)) (else (assoc key (cdr records))))) (define (make-table) (let ((local-table (list '*table*))) (define (lookup key-1 key-2) (let ((subtable (assoc key-1 (cdr local-table)))) (if subtable (let ((record (assoc key-2 (cdr subtable)))) (if record (cdr record) false)) false))) (define (insert! key-1 key-2 value) (let ((subtable (assoc key-1 (cdr local-table)))) (if subtable (let ((record (assoc key-2 (cdr subtable)))) (if record (set-cdr! record value) (set-cdr! subtable (cons (cons key-2 value) (cdr subtable))))) (set-cdr! local-table (cons (list key-1 (cons key-2 value)) (cdr local-table))))) 'ok) (define (dispatch m) (cond ((eq? m 'lookup-proc) lookup) ((eq? m 'insert-proc!) insert!) (else (error "Unknown operation -- TABLE" m)))) dispatch)) (define operation-table (make-table)) (define get (operation-table 'lookup-proc)) (define put (operation-table 'insert-proc!)) ;;;-----------
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
; TODO: investigate the lambda calculus
  
  




