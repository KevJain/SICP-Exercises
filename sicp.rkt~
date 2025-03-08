#lang sicp
(define (sqrt-iter prev guess x)
  (if (new-good-enough? prev guess)
      guess
      (sqrt-iter guess
                 (improve guess x)
                 x)))
(define (cbrt-iter guess x)
  (if (good-enough-cb? guess x)
      guess
      (cbrt-iter (improve-cbrt guess x)
                 x)))
(define (improve-cbrt guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))
(define (good-enough-cb? guess x)
  (< (abs (- (cube guess) x)) 0.001))
(define (improve guess x)
  (average guess (/ x guess)))
(define (square x) (* x x))
(define (average x y)
  (/ (+ x y) 2))


(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (new-good-enough? prev guess)
  (< (abs (- (/ prev guess) 1)) 0.001))
(define (sqrt x)
  (sqrt-iter 0 1.0 x))
(define (cube x) (* x x x))
(define (cbrt x)
  (cbrt-iter 1.0 x))

(define (factorial n)
  (if (= n 1)
      n
      (* n (factorial (- n 1)))))
(define (fact-iter counter cur n)
  (if (= counter n)
      (* counter cur)
      (fact-iter (+ counter 1)
                 (* counter cur)
                 n)))
(define (factorial-iter n)
  (fact-iter 1 1 n))
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f-iter n)
  (if (< n 3)
      n
      (f-iter-helper 2 1 0 3 n)))
(define (f-iter-helper fn-1 fn-2 fn-3 counter n)
  (if (> counter n)
      fn-1
      (f-iter-helper (+ fn-1
                        (* 2 fn-2)
                        (* 3 fn-3))
                     fn-1
                     fn-2
                     (+ counter 1)
                     n)))

(define (pascals row col)
  (cond ((or (< col 0) (> col row)) 0)
        ((= row 0) 1)
        (else (+ (pascals (- row 1) (- col 1))
                 (pascals (- row 1) col)))))
(define (pow-iter x n) (pow-iter-helper 1 x n))
(define (pow-iter-helper a x n)
  (if (= n 0)
      a
      (if (= (remainder n 2) 1)
          (pow-iter-helper (* a x) x (- n 1))
          (pow-iter-helper a (square x) (/ n 2)))))

(define (gcd a b)
  (cond ((< a b) (gcd b a))
        ((= b 0) a)
        (else (gcd b (remainder a b)))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))
(define (double x)
  (* 2 x))
(define (halve x)
  (/ x 2))

(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-mult (double a) (halve b)))
        (else (+ a (fast-mult a (- b 1))))))

(define (fast-mult-iter a b)
  (define (helper x a b)
    (cond ((= b 0) x)
          ((even? b) (helper x (double a) (halve b)))
          (else (helper (+ x a) a (- b 1)))))
  (helper 0 a b))

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square q) (square p))      ; compute p'
                   (+ (* 2 p q) (square q))      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (fibonacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (and (= n (smallest-divisor n))
       (not (= n 1)))
  )

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))    
(define (expmod-miller base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (check1 (remainder (square (expmod-miller base (/ exp 2) m))
                    m)))
        (else
         (remainder (* base (expmod-miller base (- exp 1) m))
                    m))))    

(define (check1 n)
  (if (= n 1)
      0
      n))

(define (miller-rabin n)
  (define (try-it a)
    (= (expmod-miller a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin n) (fast-prime? n (- times 1)))
        (else false)))

(define (search-for-primes start end)
  (cond ((even? start) (search-for-primes (+ start 1) end))
        ((> start end) (display "done"))
        (else (timed-prime-test start) (search-for-primes (+ start 2) end))))

(define (carmichael n)
  (define (helper n test)
    (cond ((= test n) true)
          ((= (expmod test n n) test) (helper n (+ test 1)))
          (else false)))
  (helper n 1))

; END OF SECTION 1.2

; naive
;(search-for-primes 1000 1050) ;1009, 1013, 1019, *** 2
;(search-for-primes 10000 10050) ; 10007, 10009, 10037 *** 5
;(search-for-primes 100000 100100) ; 100003, 100019, 100043 *** 8
;(search-for-primes 1000000 1001000) ;1000003, 1000033, 1000037 *** 32

; better divisor search
;(search-for-primes 1000 1050) ;1009, 1013, 1019, *** 1
;(search-for-primes 10000 10050) ; 10007, 10009, 10037 *** 3
;(search-for-primes 100000 100100) ; 100003, 100019, 100043 *** 5
;(search-for-primes 1000000 1001000) ;1000003, 1000033, 1000037 *** 16

; fermat method
;(search-for-primes 1000 1050) ;1009, 1013, 1019, *** 1
;(search-for-primes 10000 10050) ; 10007, 10009, 10037 *** 2
;(search-for-primes 100000 100100) ; 100003, 100019, 100043 *** 2
;(search-for-primes 1000000 1001000) ;1000003, 1000033, 1000037 *** 2

; BEGIN SECTION 1.3

;1.29
(define (sum term next start end)
  (if (> start end)
      0
      (+ (term start)
         (sum term next (next start) end))))

(define (id x) x)
(define (double_inc x) (+ x 2))
(define (inc x) (+ x 1))

(define (sum_it term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-it term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial-r n)
  (product id 1 inc n))

;1.31
(define (approx-term k)
  (/ (* (+ (floor (/ k 2)) 1) 2)
     (+ 1 (* (ceiling (/ k 2)) 2))))

(define (pi-approx n)
  (product approx-term 1 inc n))

;1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate-it combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

;1.33
(define (accumulate-filter combiner null-value term a next b filter)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a)
                    (accumulate-filter combiner null-value term (next a) next b filter)))
        (else (accumulate-filter combiner null-value term (next a) next b filter))))

;(accumulate + 0 id 1 inc 5)
;(accumulate-it + 0 id 1 inc 5)
;(accumulate-filter + 0 id 1 inc 5 true-filter)
;(accumulate-filter + 0 id 1 inc 5 false-filter)

(define (sum-prime-squares a b)
  (accumulate-filter + 0 square a inc b prime?))

(define (relatively-prime a b)
  (if (= (gcd a b) 1)
      #t
      #f))

(define (relatively-prime-prod n)
  (define (relatively-prime-to-n a)
    (relatively-prime n a))
  (accumulate-filter * 1 id 1 inc n relatively-prime-to-n))

;1.35
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (< (abs (- next guess)) tolerance)
          guess
          (try next))))
  (try first-guess))

(define (fixed-point-def f first-guess)
  (define (try guess)
    (define next (f guess))
      (if (< (abs (- next guess)) tolerance)
          guess
          (try next)))
  (try first-guess))

;(fixed-point (lambda (x) (+ 1 (/ 1 x)))
;             0.5)

(define (fixed-point-print f first-guess)
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (< (abs (- next guess)) tolerance)
          guess
          (try next))))
  (try first-guess))

;1.36
;(display "undamped")
;(newline)
;(fixed-point-print (lambda (x) (/ (log 1000) (log x)))
;                   2)

;(display "damped")
;(newline)
;(fixed-point-print (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2))
;                   2)

;1.37
(define (cont-frac n d k)
  (define (cont-recur i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (cont-recur (+ i 1))))))
  (cont-recur 1))

(define (cont-frac-it n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter (- k 1) (/ (n k) (d k))))

;(cont-frac (lambda (i) 1.0)
;           (lambda (i) 1.0)
;           12)

;(cont-frac-it (lambda (i) 1.0)
;           (lambda (i) 1.0)
;           12)

;1.38
;(cont-frac (lambda (i) 1.0)
;           (lambda (i) (if (= (remainder i 3) 2)
;                           (* (+ (floor (/ i 3)) 1) 2)
;                           1))
;           5)

;1.39
(define (tan-cf x k)
  (define (cont-frac-minus n d)
    (define (cont-recur i)
      (if (= i k)
          (/ (n i) (d i))
          (/ (n i) (- (d i) (cont-recur (+ i 1))))))
    (cont-recur 1))
  (cont-frac-minus (lambda (i) (if (= i 1)
                                   x
                                   (* x x)))
                   (lambda (i) (- (* 2 i) 1))))


(exact->inexact (tan-cf 1 10))
      
  

  


  















