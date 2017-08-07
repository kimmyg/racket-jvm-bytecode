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

; this should cooperate with "summary.rkt" since the form of each `l` is dictated by it
; the environment could be a procedure
(define (resolve l ls)
  (match l
    [(? exact-integer? l) l]
    ['null l]
    [(? string?) l]
    [`(local ,n)
     (hash-ref ls n l)]
    [`(new ,_) l]
    [`(newarray ,t ,l)
     `(newarray ,t ,(resolve l ls))]
    [`(stack-slot ,_) l]
    [`(arraylength ,l)
     `(arraylength ,(resolve l ls))]
    [`(access-token ,_) l]
    [`(result ,_ ,_) l]
    [`(checkcast ,l ,c)
     `(checkcast ,(resolve l ls) ,c)]
    [`(unop ,op ,l)
     `(unop ,op ,(resolve l ls))]
    [`(binop ,op ,l₀ ,l₁)
     `(binop ,op ,(resolve l₀ ls) ,(resolve l₁ ls))]))

(define (locals-summary-sequence* ls₀ ls₁)
  (let* ([ls (for/hasheqv ([(n l) (in-hash ls₁)])
               (values n (resolve l ls₀)))]
         [ls (for*/fold ([ls ls]) ([(n l) (in-hash ls₀)]
                                   #:unless (hash-has-key? ls₁ n))
               (hash-set ls n l))])
    ls))

(define (locals-summary-sequence ls₀ . lss)
  (match lss
    [(list) ls₀]
    [(cons ls lss)
     (apply locals-summary-sequence (locals-summary-sequence* ls₀ ls) lss)]))

(provide make-locals locals-ref locals-set!
         locals-summary
         locals-summary-sequence)
