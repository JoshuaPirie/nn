#lang racket

(require matrix
         "mb-constructors.rkt"
         "mb-operations.rkt")

(define (gen-xor-training n)
  (build-list n
              (λ (x) (let ([a (random 2)] [b (random 2)])
                       (list (list a b) (list (bitwise-xor a b)))))))

(define (gen-neg-training n)
  (build-list n
              (λ (x) (let ([a (random 2)])
                       (list (list a) (list (if (= a 1) 0 1)))))))

(define xor-mb (build-mb 2 2 1 (thunk* (sub1 (* (random) 2)))))
(define trained-xor-mb (mb-train-all xor-mb 0.01 (gen-xor-training 10000)))

(string-append (number->string (first (mb-feedforward trained-xor-mb '(0 0)))) " : 0")
(string-append (number->string (first (mb-feedforward trained-xor-mb '(0 1)))) " : 1")
(string-append (number->string (first (mb-feedforward trained-xor-mb '(1 0)))) " : 1")
(string-append (number->string (first (mb-feedforward trained-xor-mb '(1 1)))) " : 0")

(define neg-mb (build-mb 1 1 1 (thunk* (sub1 (* (random) 2)))))
(define trained-neg-mb (mb-train-all neg-mb 0.01 (gen-neg-training 10000)))

(string-append (number->string (first (mb-feedforward trained-neg-mb '(0)))) " : 1")
(string-append (number->string (first (mb-feedforward trained-neg-mb '(1)))) " : 0")
