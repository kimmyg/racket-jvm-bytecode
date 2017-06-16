#lang racket/base
(require racket/match)

(define (make-locals max-locals)
  (make-hasheqv))

(define (locals-ref l n)
  (hash-ref l n (λ () `(local ,n))))

(define (locals-set! l n v)
  (hash-set! l n v))

(define (locals-summary l)
  l)

(define (locals-summary-sequence ls₀ . lss)
  (match lss
    [(list) ls₀]
    [(cons ls lss)
     (apply locals-summary-sequence `(sequence ,ls₀ ,ls) lss)]))

(provide make-locals locals-ref locals-set!
         locals-summary
         locals-summary-sequence)
