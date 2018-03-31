#lang racket

(require matrix
         "mb-types.rkt"
         "mb-utils.rkt")

(provide (all-defined-out))

(define (add-bias l)
  (col-matrix (append l (list 1))))

(define (sigmoid x)
  (/ 1 (add1 (exp (- x)))))

(define (feedforward m i)
  (matrix-col (matrix-map sigmoid (matrix* m (add-bias i))) 0))

(define/contract (mb-feedforward n i)
  (mb? matrix-col? . -> . matrix-col?)
  (if (= (mb-num-inputs n) (length i))
      (feedforward (mb-ho n) (feedforward (mb-ih n) i))
      (raise-arguments-error 'mb-feedforward
                             "number of given inputs does not match input nodes of motherbrain"
                             "given inputs" (length i)
                             "motherbrain input nodes" (mb-num-inputs n))))

(define (adjusted-weights w i o e lr)
  (matrix+ (matrix* (matrix-hadamard lr e
                                     (matrix-map (λ (x) (* x (- 1 x)))
                                                 (col-matrix o)))
                    (matrix-transpose (add-bias i)))
           w))

(define/contract (mb-train n lr i t)
  (mb? (and/c number? (>=/c 0)) matrix-col? matrix-col? . -> . mb?)
  (if (= (mb-num-inputs n) (length i))
      (if (= (mb-num-outputs n) (length t))
          (let* ([hidden-output (feedforward (mb-ih n)
                                             i)]
                 [output-output (feedforward (mb-ho n)
                                             hidden-output)]
                 [output-error (matrix- (col-matrix t)
                                        (col-matrix output-output))]
                 [hidden-error (matrix* (drop-right (matrix-transpose (mb-ho n)) 1)
                                        output-error)])
            (mb (adjusted-weights (mb-ih n) i hidden-output hidden-error lr)
                (adjusted-weights (mb-ho n) hidden-output output-output output-error lr)))
          (raise-arguments-error 'mb-train
                                 "number of given targets does not match output nodes of motherbrain"
                                 "given targets" (length t)
                                 "motherbrain output nodes" (mb-num-outputs n)))
      (raise-arguments-error 'mb-train
                             "number of given inputs does not match input nodes of motherbrain"
                             "given inputs" (length i)
                             "motherbrain input nodes" (mb-num-inputs n))))

(define/contract (mb-train-all n lr itp)
  (mb? (and/c number? (>=/c 0)) (listof (cons/c matrix-col? (cons/c matrix-col? empty?))) . -> . mb?)
  (if (= (length itp) 0)
      n
      (mb-train-all (mb-train n
                              (* lr (sqrt (length itp)))
                              (first (first itp))
                              (second (first itp)))
                    lr (rest itp))))
