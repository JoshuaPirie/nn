#lang racket

(require matrix
         "mb-types.rkt")

(provide (all-defined-out))

(define/contract (make-mb i h o [v 0])
  ((exact-positive-integer?
    exact-positive-integer?
    exact-positive-integer?)
   (mb-weight-val?) . ->* . mb?)
  (mb (make-matrix h (add1 i) v) (make-matrix o (add1 h) v)))

(define/contract (build-mb i h o [f (const 0)])
  ((exact-positive-integer?
    exact-positive-integer?
    exact-positive-integer?)
   ((exact-nonnegative-integer?
     exact-nonnegative-integer? . -> . mb-weight-val?)) . ->* . mb?)
  (mb (build-matrix h (add1 i) f) (build-matrix o (add1 h) f)))
