#lang racket


; (cartesian-product '(a b) '(1 2))
; ((a 1) (a 2) (b 1) (b 2))


(define (listify x)
  (if (list? x)
      x
      (list x)))

(define (cartesian-product x y)
  (let ([val '()])
    (for-each 
     (lambda (a)
       (for-each ; seconda lista
        (lambda (b)
          (set! val
                (append val
                        (list (append
                               (listify a)
                               (listify b))))))
        y))
     x)
    val))



; (for x in '(1 2 3) (display x)(newline))

;
; (define-syntax id
;   (syntax-rules (literal-id ...)
;     [pattern template]
;     ...))
;

(define-syntax for
  (syntax-rules (in)
    [(_ var in lst fun ...)
     (for-each (lambda (var)
                 fun ...) lst)]))


; (list/co (* x y) when (even? x) from x '(1 2 3) y '(2 4 6))

; (list/co (* x y) from x '(1 2 3) y '(2 4 6))
; (list/co (* x x) from x '(1 2 3))

(define (concat-map lst f)
  (apply append (map f lst)))


(define-syntax list/co
  (syntax-rules (when from)
    
    [(_ expr from v1 l1)
     (concat-map l1
                 (lambda (v1)
                   (list expr)))]
    
    [(_ expr from v1 l1 v2 l2 ...)
     (concat-map l1 
                 (lambda (v1)
                   (list/co expr from v2 l2 ...)))]
    
    [(_ expr when condition from v1 l1)
     (concat-map l1
                 (lambda (v1)
                   (if condition
                       (list expr)
                       '())))]
    
    [(_ expr when condition from v1 l1 v2 l2 ...)
     (concat-map l1
                 (lambda (v1)
                   (list/co expr when condition 
                            from v2 l2 ...)))]))



(define *queue* '())


(define (empty-queue?)
  (null? *queue*))


(define (enqueue x)
  (set! *queue* (append *queue* (list x))))


(define (dequeue)
  (let ([x (car *queue*)])
    (set! *queue* (cdr *queue*))
    x))

(define (fork proc)
  (call/cc
   (lambda (k)
     (enqueue k)
     (proc))))


(define (yield)
  (call/cc
   (lambda (k)
     (enqueue k)
     ((dequeue)))))

(define (thread-exit)
  (if (empty-queue?)
      (exit)
      ((dequeue))))


(define (do-stuff-n-print str max)
  (lambda ()
    (let loop ([n 0])
      (display str)(display " ")(display n)(newline)
      (yield)
      (if 
       (< n max)
       (loop (+ 1 n))
       (thread-exit)))))


(fork (do-stuff-n-print "This is A" 4))
(fork (do-stuff-n-print "This is B" 5))
(thread-exit)
