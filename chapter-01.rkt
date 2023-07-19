#lang sicp

;;; Chapter 1

;;; Exercise 1.1

10 ; 10
(+ 5 3 4) ; 12
(- 9 1) ; 8
(/ 6 2) ; 3
(+ (* 2 4) (- 4 6)) ; 6
(define a 3) ; no visible output, a = 3
(define b (+ a 1)) ; no visible output, b = 4
(+ a b (* a b)); 19
(= a b) ; #f
(if (and (> b a) (< b (* a b)))
    b
    a) ; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; 16
(+ 2 (if (> b a) b a)) ; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ; 16

;;; Exercise 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;;; Exercise 1.3

(define (sq x) (* x x))
(define (sum-sq x y) (+ (sq x) (sq y)))
(define (sum-sq-lrg x y z)
  (cond ((and (>= x z) (>= y z)) (sum-sq x y))
        ((and (>= x y) (>= z y)) (sum-sq x z))
        (else (sum-sq y z))))

;;; Exercise 1.4

; Returns a + |b|.

;;; Exercise 1.5

; When the function p evaluates, it calls itself, which leads to an infinite loop.
; An applicative-order interpreter will evaluate all the arguments, including p, which leads to an infinite loop.
; A normal-order interpreter will evaluate the argument values as needed, and (test 0) will resolve when when (= x 0) returns #t.

;;; Exercise 1.6

; When the function new-if is called, it evaluates all its arguments, and so sqrt-iter keeps calling itself
; which leads to an infinite loop.

;;; Exercise 1.7

(define (percent-change old-value new-value)
      (/ (- new-value old-value) old-value))

(define (good-enough? old-guess new-guess)
      (< (abs (percent-change old-guess new-guess)) 0.00001))

(define (average x y)
      (/ (+ x y) 2))

(define (improve guess x)
      (average guess (/ x guess)))

(define (sqrt-iter-improved old-guess new-guess x)
      (if (good-enough? old-guess new-guess)
      new-guess
      (sqrt-iter-improved new-guess (improve new-guess x) x)))

(define (sqrt x)
      (sqrt-iter-improved x 1.0 x))

;;; Exercise 1.8

(define (cbrt-improve guess x)
      (/ (+ (/ x (* guess guess)) (* 2 guess))
      3))

(define (cbrt-iter-improved old-guess new-guess x)
      (if (good-enough? old-guess new-guess)
      new-guess
      (cbrt-iter-improved new-guess (cbrt-improve new-guess x) x)))

(define (cbrt x)
      (cbrt-iter-improved x 1.0 x))

;;; Exercise 1.9

; The first definition is recursive:
; (+ 4 5) =>
; (inc (+ 3 5)) =>
; (inc (inc (+ 2 5))) =>
; (inc (inc (inc (+ 1 5)))) =>
; (inc (inc (inc (inc (+ 0 5))))) =>
; (inc (inc (inc (inc 5)))) =>
; (inc (inc (inc 6))) =>
; (inc (inc 7)) =>
; (inc 8) =>
; 9

; The second definition is iterative:
; (+ 4 5) =>
; (+ 3 6) =>
; (+ 2 7) =>
; (+ 1 8) =>
; (+ 0 9) =>
; 9

;;; Exercise 1.10

; (f n) => 2 * n
; (g n) => 0 if n = 0, otherwise (f (f (f ... 1))) n times, i.e., 2^n
; (h n) => 0 if n = 0, otherwise (g (g (g ... 1))) n times
; (k n) => 5 * n^2

;;; Exercise 1.11

(define (f-iter-helper f-n-1 f-n-2 f-n-3 counter n)
  (if (> counter n)
      f-n-1
      (f-iter-helper (+ f-n-1 (* 2 f-n-2) (* 3 f-n-3)) f-n-1 f-n-2 (inc counter) n)))

(define (f-iter n)
  (if (< n 3)
      3
      (f-iter-helper 3 3 3 1 (- n 2))))

(define (f-recr n)
  (if (< n 3)
      3
      (+ (f-recr (- n 1)) (* 2 (f-recr (- n 2))) (* 3 (f-recr (- n 3))))))

;;; Exercise 1.12

(define (binomial n k)
  (if (or (<= k 0) (>= k n))
      1
      (+ (binomial (- n 1) (- k 1)) (binomial (- n 1) k))))

;;; Exercise 1.13

; I have found a truly wonderful proof, but the margin is too small to contain it.