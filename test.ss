#lang scheme
(require scheme/package
         tests/eli-tester
         "fib-heap.ss")

(define (fib-heap-insert*! f l)
  (for-each (lambda (i) (fib-heap-insert! f i)) l))

(define (fib-heap->list f)
  (fib-heap-map! f (lambda (i) i)))

(package-begin
 (define fh1 (make-fib-heap <))
 (fib-heap-insert*! fh1 (list 10 11 6 14 1 0))
 (test 
  (fib-heap->list fh1) => (list 0 1 6 10 11 14)))

(package-begin
 (define fh1 (make-fib-heap <))
 (define fh1-list (list 3 9 5 8 1 0 3 4))
 (fib-heap-insert*! fh1 fh1-list)
 (define fh2 (make-fib-heap <))
 (define fh2-list (list 91 1294 1284 41 46 1))
 (fib-heap-insert*! fh2 fh2-list)
 (fib-heap-union! fh1 fh2)
 (define fh0 (make-fib-heap <))
 (fib-heap-insert*! fh0 (append fh1-list fh2-list)) 
 (test
  (fib-heap->list fh0) => (sort (append fh1-list fh2-list) <)
  (fib-heap->list fh1) => (sort (append fh1-list fh2-list) <)))