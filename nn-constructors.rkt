#lang racket

(require matrix
         "nn-types.rkt")

(provide (all-defined-out))

(define/contract (make-nn i h o [v 0])
  ((exact-positive-integer?
    exact-positive-integer?
    exact-positive-integer?)
   (nn-weight-val?) . ->* . nn?)
  (nn (make-matrix h (add1 i) v) (make-matrix o (add1 h) v)))

(define/contract (build-nn i h o [f (const 0)])
  ((exact-positive-integer?
    exact-positive-integer?
    exact-positive-integer?)
   ((exact-nonnegative-integer?
     exact-nonnegative-integer? . -> . nn-weight-val?)) . ->* . nn?)
  (nn (build-matrix h (add1 i) f) (build-matrix o (add1 h) f)))
