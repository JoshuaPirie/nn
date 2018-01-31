#lang racket

(require matrix
         "nn-types.rkt")

(provide (all-defined-out))

(define/contract (nn-num-inputs n)
  (nn? . -> . exact-positive-integer?)
  (sub1 (matrix-num-cols (nn-ih n))))

(define/contract (nn-num-hidden n)
  (nn? . -> . exact-positive-integer?)
  (matrix-num-rows (nn-ih n)))

(define/contract (nn-num-outputs n)
  (nn? . -> . exact-positive-integer?)
  (matrix-num-rows (nn-ho n)))
