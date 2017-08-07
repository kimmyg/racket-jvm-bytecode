#lang racket/base
(require racket/list
         racket/match)

(struct SS (offset array) #:mutable)

(define (make-stack max-stack)
  (SS 0 null))

(define (stack-push! s . vs)
  (match-define (SS offset array) s)
  (set-SS-offset! s (- offset (length vs)))
  (set-SS-array! s (append vs array)))

(define (ensure array offset)
  (if (null? array)
    (list `(stack-slot ,offset))
    array))

(define (stack-pop!* s [n 0])
  (match-define (SS offset array) s)
  (let loop ([n n]
             [offset offset]
             [array array])
    (if (zero? n)
      (begin
        (set-SS-offset! s offset)
        (set-SS-array! s array)
        (list))
      (match-let ([(cons v array) (ensure array offset)]
                  [offset (add1 offset)])
        (cons v (loop (sub1 n) offset array))))))

(define (stack-pop! s)
  (match-define (list v) (stack-pop!* s 1))
  v)

(struct stack-summary (offset array) #:transparent)

(define make-stack-summary
  (match-lambda
    [(SS offset array)
     (stack-summary offset array)]))

(define collapse-adjacent-stack-summaries
  (match-lambda**
   [((stack-summary offset₀ array₀)
     (stack-summary 0 (list)))
    (stack-summary offset₀ array₀)]
   [((stack-summary 0 (list))
     (stack-summary offset₁ array₁))
    (stack-summary offset₁ array₁)]
   [((stack-summary -1 (list x))
     (stack-summary 1 (list)))
    (stack-summary (+ -1 1) (drop (list x) 1))]
   [((stack-summary (? (λ (x) (<= x 0)) offset₀) array₀)
     (stack-summary (? (λ (x) (<= x 0)) offset₁) array₁))
    (stack-summary (+ offset₀ offset₁) (append array₁ array₀))]
   [((stack-summary (? (λ (x) (< x 0)) offset₀) array₀)
     (stack-summary (? (λ (x) (> x 0)) offset₁) array₁))
    (if (<= (+ offset₀ offset₁) 0)
      (stack-summary (+ offset₀ offset₁) (drop array₀ offset₁))
      (failure-cont))]
   [((stack-summary offset₀ array₀)
     (stack-summary offset₁ array₁))
    (raise (list 'stack-summary offset₀ array₀ offset₁ array₁))]))

#;
(define (stack-summary-sequence ss₀ . sss)
  (match sss
    [(list) ss₀]
    [(cons ss sss)
     (apply stack-summary-sequence `(sequence ,ss₀ ,ss) sss)]))

(provide make-stack stack-push! stack-pop! stack-pop!*
         (struct-out stack-summary) make-stack-summary
         collapse-adjacent-stack-summaries #;stack-summary-sequence
         )
