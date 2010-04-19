#lang scheme
; http://www.cs.princeton.edu/~wayne/cs423/fibonacci/FibonacciHeapAlgorithm.html

;; A fib-heap-node represents a single node in the heap
;; Nodes are stored in a circular linked-list and may point
;; to a child node
(define-struct node (data parent left right child degree) #:mutable)

;; The heap itself keeps track of its size, the minimum node (i.e. the root of the tree)
;; and a function to compare nodes.
(define-struct fib-heap (size min <) #:mutable)

(define (node-for-each f n)
  (define start n)
  (f n)
  (let loop ([n (node-left n)])
    (unless (eq? n start)
      (f n)
      (loop (node-left n)))))

(define (node-length n)
  (define len 0)
  (node-for-each (lambda _ (set! len (add1 len))) n)
  len)

(define (node->list n)
  (define l empty)
  (node-for-each (lambda (n) (set! l (list* n l))) n)
  (reverse l))

(define (create-node data)
  (shared ([new-node (make-node data #f new-node new-node #f 0)])
    new-node))

(define (node-splice! l1 l2)
  (set-node-left! (node-right l1) l2)
  (set-node-left! (node-right l2) l1)
  (let ([temp (node-right l1)])
    (set-node-right! l1 (node-right l2))
    (set-node-right! l2 temp)))

(define (node-insert! l n)
  (set-node-left! n n)
  (set-node-right! n n)
  (node-splice! l n))

(define (node-add-child! n cn)
  (if (node-child n)
      (node-splice! (node-child n) cn)
      (set-node-child! n cn))
  (set-node-parent! cn n)
  (set-node-degree! n (add1 (node-degree n))))

(define (node-unlink! n)
  (if (eq? n (node-left n))
      #f
      (begin
        (set-node-right! (node-left n) (node-right n))
        (set-node-left! (node-right n) (node-left n))
        (set-node-left! n n)
        (set-node-right! n n))))

(define (build-fib-heap <)
  (make-fib-heap 0 #f <))

(define (fib-heap-insert! h d)
  (define min (fib-heap-min h))
  (define n (create-node d))
  (set-fib-heap-size! h (add1 (fib-heap-size h)))
  (if min
      (begin (node-splice! min n)
             (when ((fib-heap-< h) d (node-data min))
               (set-fib-heap-min! h n)))
      (set-fib-heap-min! h n)))

; merges h2 into h1
(define (fib-heap-union! h1 h2)
  (define min1 (fib-heap-min h1))
  (define min2 (fib-heap-min h2))
  (cond
    [(and min1 min2)      
     (node-splice! min1 min2)
     (when ((fib-heap-< h1) (node-data min2) (node-data min1))
       (set-fib-heap-min! h1 min2))
     (set-fib-heap-size! h1 (+ (fib-heap-size h1) (fib-heap-size h2)))]
    [min1
     (void)]
    [min2
     (set-fib-heap-size! h1 (fib-heap-size h2))
     (set-fib-heap-min! h1 (fib-heap-min h2))
     (set-fib-heap-<! h1 (fib-heap-< h2))]))

(define (fib-heap-extract-min! h)
  (define z (fib-heap-min h))
  (if (not z)
      #f
      (let ([cz (node-child z)])
        (when cz
          ; Reset parents of children
          (node-for-each (lambda (n) (set-node-parent! n #f))
                         cz)
          ; Merge child with heap
          (node-splice! z cz))
        ; Remove it from the list
        (let ([z-right (node-right z)])
          (if (node-unlink! z)
              (set-fib-heap-min! h z-right)
              (set-fib-heap-min! h cz)))
        ; Consolidate
        (when (fib-heap-min h)
          (fib-heap-consolidate! h))
        ; Decrease size
        (set-fib-heap-size! h (sub1 (fib-heap-size h)))
        (node-data z))))

(define (node-link! x y)
  (node-unlink! y)
  (node-add-child! x y))

(define (logn x y)
  (/ (log x) (log y)))

(define (fib-heap-consolidate! h)
  (define dh (inexact->exact (ceiling (logn (fib-heap-size h) 2))))
  (define a (make-vector (add1 dh) #f))  
  
  (for ([w (node->list (fib-heap-min h))])
    (let* ([x w]
           [d (node-degree x)])
      (let loop ([y (vector-ref a d)])
        (when y
          (when ((fib-heap-< h) (node-data y) (node-data x))
            (let ([temp x])
              (set! x y)
              (set! y temp)))
          (node-link! x y)
          (vector-set! a d #f)
          (set! d (add1 d))
          (loop (vector-ref a d))))
      (vector-set! a d x)))
  
  (set-fib-heap-min! h #f)
  (for ([n (in-vector a)])
    (when n
      (fib-heap-consolidate*! h n))))

(define (fib-heap-consolidate*! h n)
  (set-node-left! n n)
  (set-node-right! n n)
  (cond 
    [(fib-heap-min h)
     => (lambda (min)
          (node-splice! min n)
          (when ((fib-heap-< h) (node-data n) (node-data min))
            (set-fib-heap-min! h n)))]
    [else
     (set-fib-heap-min! h n)]))

(define (fib-heap-map! h f)
  (let loop ()
    (cond
      [(fib-heap-extract-min! h)
       => (lambda (d) (list* (f d) (loop)))]
      [else
       empty])))

(define (fib-heap-for-each! h f)
  (let loop ()
    (cond
      [(fib-heap-extract-min! h)
       => (lambda (d) (f d) (loop))]
      [else
       empty])))

(provide/contract
 [fib-heap? (any/c . -> . boolean?)]
 [rename build-fib-heap make-fib-heap
         ((any/c any/c . -> . boolean?) . -> . fib-heap?)]
 [fib-heap-size (fib-heap? . -> . exact-nonnegative-integer?)]
 [fib-heap-extract-min! (fib-heap? . -> . (or/c any/c false/c))]
 [fib-heap-insert! (fib-heap? any/c . -> . void)]
 [fib-heap-union! (fib-heap? fib-heap? . -> . void)]
 [fib-heap-map! (fib-heap? (any/c . -> . any/c) . -> . (listof any/c))]
 [fib-heap-for-each! (fib-heap? (any/c . -> . void) . -> . void)])