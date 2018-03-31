#lang racket

(require matrix
         "mb-types.rkt")

(provide (all-defined-out))

(define/contract (mb-num-inputs n)
  (mb? . -> . exact-positive-integer?)
  (sub1 (matrix-num-cols (mb-ih n))))

(define/contract (mb-num-hidden n)
  (mb? . -> . exact-positive-integer?)
  (matrix-num-rows (mb-ih n)))

(define/contract (mb-num-outputs n)
  (mb? . -> . exact-positive-integer?)
  (matrix-num-rows (mb-ho n)))
