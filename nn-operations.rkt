#lang racket

(require matrix
         "nn-types.rkt"
         "nn-utils.rkt")

(provide (all-defined-out))

(define (add-bias l)
  (col-matrix (append l (list 1))))

(define (sigmoid x)
  (/ 1 (add1 (exp (- x)))))

(define (feedforward m i)
  (matrix-col (matrix-map sigmoid (matrix* m (add-bias i))) 0))

(define/contract (nn-feedforward n i)
  (nn? matrix-col? . -> . matrix-col?)
  (if (= (nn-num-inputs n) (length i))
      (feedforward (nn-ho n) (feedforward (nn-ih n) i))
      (raise-arguments-error 'nn-feedforward
                             "number of given inputs does not match input nodes of nn"
                             "given inputs" (length i)
                             "nn input nodes" (nn-num-inputs n))))

(define (adjusted-weights w i o e lr)
  (matrix+ (matrix* (matrix-hadamard lr e
                                     (matrix-map (λ (x) (* x (- 1 x)))
                                                 (col-matrix o)))
                    (matrix-transpose (add-bias i)))
           w))

(define/contract (nn-train n lr i t)
  (nn? (and/c number? (>=/c 0)) matrix-col? matrix-col? . -> . nn?)
  (if (= (nn-num-inputs n) (length i))
      (if (= (nn-num-outputs n) (length t))
          (let* ([hidden-output (feedforward (nn-ih n)
                                             i)]
                 [output-output (feedforward (nn-ho n)
                                             hidden-output)]
                 [output-error (matrix- (col-matrix t)
                                        (col-matrix output-output))]
                 [hidden-error (matrix* (drop-right (matrix-transpose (nn-ho n)) 1)
                                        output-error)])
            (nn (adjusted-weights (nn-ih n) i hidden-output hidden-error lr)
                (adjusted-weights (nn-ho n) hidden-output output-output output-error lr)))
          (raise-arguments-error 'nn-train
                                 "number of given targets does not match output nodes of nn"
                                 "given targets" (length t)
                                 "nn output nodes" (nn-num-outputs n)))
      (raise-arguments-error 'nn-train
                             "number of given inputs does not match input nodes of nn"
                             "given inputs" (length i)
                             "nn input nodes" (nn-num-inputs n))))

(define/contract (nn-train-all n lr itp)
  (nn? (and/c number? (>=/c 0)) (listof (cons/c matrix-col? (cons/c matrix-col? empty?))) . -> . nn?)
  (if (= (length itp) 0)
      n
      (nn-train-all (nn-train n
                              (* lr (sqrt (length itp)))
                              (first (first itp))
                              (second (first itp)))
                    lr (rest itp))))
