#lang racket

(require matrix
         "nn-constructors.rkt"
         "nn-operations.rkt")

(define (gen-xor-training n)
  (build-list n
              (λ (x) (let ([a (random 2)] [b (random 2)])
                       (list (list a b) (list (bitwise-xor a b)))))))

(define (gen-neg-training n)
  (build-list n
              (λ (x) (let ([a (random 2)])
                       (list (list a) (list (if (= a 1) 0 1)))))))

(define xor-nn (build-nn 2 2 1 (thunk* (sub1 (* (random) 2)))))
(define trained-xor-nn (nn-train-all xor-nn 0.01 (gen-xor-training 10000)))

(string-append (number->string (first (nn-feedforward trained-xor-nn '(0 0)))) " : 0")
(string-append (number->string (first (nn-feedforward trained-xor-nn '(0 1)))) " : 1")
(string-append (number->string (first (nn-feedforward trained-xor-nn '(1 0)))) " : 1")
(string-append (number->string (first (nn-feedforward trained-xor-nn '(1 1)))) " : 0")

(define neg-nn (build-nn 1 1 1 (thunk* (sub1 (* (random) 2)))))
(define trained-neg-nn (nn-train-all neg-nn 0.01 (gen-neg-training 10000)))

(string-append (number->string (first (nn-feedforward trained-neg-nn '(0)))) " : 1")
(string-append (number->string (first (nn-feedforward trained-neg-nn '(1)))) " : 0")
