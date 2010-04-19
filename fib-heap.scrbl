#lang scribble/doc
@(require (planet cce/scheme:4:1/planet)
          scribble/manual
          (for-label scheme/base
                     scheme/contract
                     "main.ss"))

@title{Fibonacci Heap}
@author{@(author+email "Jay McCarthy" "jay@plt-scheme.org")}

@defmodule/this-package[]

This package provides an imperative @link["http://en.wikipedia.org/wiki/Fibonacci_heap"]{Fibonacci heap} implementation based on
@link["http://www.cs.princeton.edu/~wayne/cs423/fibonacci/FibonacciHeapAlgorithm.html"]{this implementation}.

@defproc[(fib-heap? [v any/c])
         boolean?]{
 Determines if @scheme[v] is a heap.
}

@defproc[(make-fib-heap [< (any/c any/c . -> . boolean?)])
         fib-heap?]{
 Constructs a heap that uses @scheme[<] to compare elements.
}
                   
@defproc[(fib-heap-size [fh fib-heap?])
         exact-nonnegative-integer?]{
 Returns the number of elements in @scheme[fh].
}
                                    
@defproc[(fib-heap-extract-min! [fh fib-heap?])
         (or/c any/c false/c)]{
 Extracts the minimum element from @scheme[fh] or returns @scheme[#f] if @scheme[(zero? (fib-heap-size fh))].
}
                              
@defproc[(fib-heap-insert! [fh fib-heap?] [v any/c])
         void]{
 Inserts @scheme[v] into @scheme[fh].
}
              
@defproc[(fib-heap-union! [fh1 fib-heap?]
                          [fh2 fib-heap?])
         void]{
 Efficiently inserts all the elements of @scheme[fh2] into @scheme[fh1].
}
              
@defproc[(fib-heap-map! [fh fib-heap?]
                        [f (any/c . -> . any/c)])
         (listof any/c)]{
 Returns a list that contains the result of calling @scheme[f] on each element of @scheme[fh] in the minimum order.
}
                        
@defproc[(fib-heap-for-each! [fh fib-heap?]
                             [f (any/c . -> . void)])
         void]{
 Calls @scheme[f] on each element of @scheme[fh] in the minimum order. 
 (Note: This might go into an infinite loop if @scheme[f] calls @scheme[fib-heap-insert!])
}
