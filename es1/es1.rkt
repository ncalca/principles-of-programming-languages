#lang racket

; Nicola Calcavecchia <calcavecchia@gmail.com> - 13/03/2013

; Factorial
(define (fac n)
  (if (zero? n) ;(= n 0)
      1
      (* n (fac (- n 1)))))


; List length
(define (len l)
  (if (null? l)
      0
      (+ 1 (len (cdr l)))))

; List length using a lambda function
(define len2 (lambda (l)
               (if (null? l)
                   0
                   (+ 1 (len2 (cdr l))))))

; Sequence of numbers between lo and hi (included)
(define (sequence lo hi)
  (if (> lo hi)
      (error "lo > hi")
      (if (< lo hi)
          (cons lo (sequence (+ 1 lo) hi))
          (list lo))))
;          lo)))       ; substitute with line 28 and see what happens
; (cons lo '()))))     ; substitute with line 28 and see what happens

; Sequence using cond
(define (sequence2 lo hi)
  (cond [(> lo hi) (error "lo > hi")]
        [(< lo hi) (cons lo (sequence2 (+ 1 lo) hi))]
        [else (list lo)]))


; Fibonacci (doubly recursive!)
(define (fib1 n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [#t (+ (fib1 (- n 1)) (fib1 (- n 2)))]))


; Fibonacci with tail call
(define (fib2 n)
  (let fib [(i n)
            (fn-1 0)
            (fn-2 1)]
    (if (= i 0)
        fn-1
        (fib (- i 1) (+ fn-1 fn-2) fn-1))))

; Sums all integers in an arbitrarily nested list structure
(define (sumlist l)
  (cond [(null? l) 0]
        [(list? (car l)) (+ (sumlist (car l)) (sumlist (cdr l)))]
        [else (+ (car l) (sumlist (cdr l)))]))


; Same as before improved to ignore non-numbers
(define (sumlist2 l)
  (cond [(null? l) 0]
        [(list? (car l)) (+ (sumlist2 (car l)) (sumlist2 (cdr l)))]
        [(number? (car l)) (+ (car l) (sumlist2 (cdr l)))] ;racket is dinamically typed, we can't ensure there will be integers
        [else (sumlist2 (cdr l))]))


; Implementation of map function
(define (my-map l f)
  (if (null? l)
      '()
      (cons (f (car l)) (my-map (cdr l) f))))


; Exercises:

; 0.
; Rewrite function my-map using a named let.

; 1. 
; Define "my-map-vector" that does the same thing of the map,
; except that it processes a vector.
; You can use the following functions (defined in the standard library):
; vector-length, vector-ref.

; 2.
; Define a function "vect-to-list" that transforms a vector into a list

; 3.
; Define a function "take" that accepts a non-negative number n and a list l.
; The function returns the first n elements of the list or the whole list
; if the its length is less than n.
; You can use the function "reverse" which just reverses the order of a list

