#lang racket

; Nicola Calcavecchia <calcavecchia@gmail.com> - 22/03/2013

; Map on a vector
(define (my-map-vector v f)
  (let loop ([pos 0])
    (if (< pos (vector-length v))
        (cons (f (vector-ref v pos)) (loop (+ pos 1)))
        '())))

; Initialize a matrix with with the given number of rows and colums and element
; as the default element
(define (make-matrix rows cols fill)
  (let ([vec (make-vector rows #f)])
    (let loop ([x 0])
      (if (< x rows)
          (begin
            (vector-set! vec x (make-vector cols fill))
            (loop (+ x 1)))
          vec))))

; Counts the number of elements within an arbitrarily nested list
; (count-nodes '(1 2 3 (+ 4 5))) => 6
(define (count-nodes l)
  (define stack0 (list l))
  (let loop ([stack (cdr stack0)]
             [res 1]
             [curr (car stack0)])
    (when (list? curr)
      (for-each (lambda (x) 
                  (set! stack (cons x stack)))
                (cdr curr)))
    (if (null? stack)
        res
        (loop (cdr stack)
              (+ 1 res)
              (car stack)))))

; Computes the powerset of a given list xs
(define (all-subsets xs)
  (if (null? xs)
      '(())
      (append
       (all-subsets (cdr xs))
       (map (lambda (subset) 
              (cons (car xs) subset))
            (all-subsets (cdr xs))))))


; Computes the powerset of a given list xs (avoiding 
; recomputation of the powerset twice)
(define (all-subsets2 xs)
  (if (null? xs)
      '(())
      (let ([subsets (all-subsets2 (cdr xs))])
        (append
         subsets
         (map (lambda (subset)
                (cons (car xs) subset))
              subsets)))))

; Define a struct for an event with x,y space coordinates
; a name and the number of participants
(struct event
  (x y name (participants #:mutable)))

; Procedure that raises an error if the given condition is not true 
(define (assert c)
  (unless c 
    (error "assertion failed") ))

; Prints the fields of an event
(define (print-event e)
  (assert (event? e))
  (display (event-name e))
  (display ": coord(")
  (display (event-x e))
  (display ",")
  (display (event-y e))
  (display "), participants: ")
  (display (event-participants e))
  (newline))

; Creates an event list (initially empty)
(define event-list '())

; Add the given event to the list of events
(define (add-event-to-list event)
  (set! event-list (cons event event-list)))

; Prints all events in the list
(define (print-all-events)
  (for-each print-event event-list))

; Computes the distance of a given event from a couple of coordinates (x,y)
(define (event-distance-from e x y)
  (assert (event? e))
  (let ([e-x (event-x e)]
        [e-y (event-y e)]
        [^2 (lambda (x) (* x x))])
    (sqrt (+ (^2 (- e-x x)) (^2 (- e-y y))))))


; Returns all events close to (x,y) by a given range
(define (events-close-to x y range)
  (let ([close? (lambda (e) 
                  (< (event-distance-from e x y) range))])
    (filter close? event-list)))

; Defines a macro for an alternative versione of the
; if then else control construct (i.e., syntactic sugar)
; The macro works this way:
;      (if: condition then: exp1 else: exp2)

(define-syntax if:
  (syntax-rules (then: else:)
    ([if: e1 then: e2 else: e3]
     (if e1 e2 e3))))

; Macro to compute the max between two values
(define-syntax my-max
  (syntax-rules ()
    [( _ a b)
     (if (> a b) a b)]))

; Macro to compute the max between two values where
; the input expressions are evaluated only once
; (i.e., it solves the problem you see in the .c source files we
; saw in class)
(define-syntax my-max2
  (syntax-rules ()
    [( _ a b) 
     (let ([new-a a]
           [new-b b])
       (if (> new-a new-b) new-a new-b))]))