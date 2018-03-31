#lang racket

(require matrix)

(provide (contract-out [struct mb ([ih matrix?] [ho matrix?])])
         mb-weight-val?)

(struct mb (ih ho))

(define/contract (mb-weight-val? v)
  predicate/c
  (and (matrix-val? v)
       (>= v -1)
       (<= v 1)))
