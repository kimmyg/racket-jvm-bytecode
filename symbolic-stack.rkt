#lang racket/base
(require racket/match)

(struct SS (offset array) #:mutable)

(define (make-empty-stack)
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

(provide make-empty-stack stack-push! stack-pop! stack-pop!*)
