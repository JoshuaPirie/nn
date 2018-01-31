#lang racket

(require matrix)

(provide (contract-out [struct nn ([ih matrix?] [ho matrix?])])
         nn-weight-val?)

(struct nn (ih ho))

(define/contract (nn-weight-val? v)
  predicate/c
  (and (matrix-val? v)
       (>= v -1)
       (<= v 1)))
