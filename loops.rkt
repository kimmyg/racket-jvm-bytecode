#lang racket/base
(require racket/match
         racket/set
         "data.rkt"
         "instruction.rkt")

(struct loop (header-block-pc footer-block-pc) #:transparent)


(define (loops-as-sets dominators loop-footers comefrom)
  (for/hash ([(footer-pc header-pc) (in-hash loop-footers)])
    (values (loop header-pc footer-pc)
            (let loop ([todo (list footer-pc)]
                       [seen (seteqv)])
              (match todo
                [(cons pc todo)
                 (if (set-member? seen pc)
                   (loop todo seen)
                   (loop (for*/fold ([todo todo]) ([pred-pc (in-list (hash-ref comefrom pc))]
                                                   #:when (subset? (hash-ref dominators header-pc) (hash-ref dominators pred-pc)))
                           (cons pred-pc todo))
                         (set-add seen pc)))]
                [(list) seen])))))

(require racket/list)

(define (loops-as-paths dominators loop-footers comefrom)
  ((current-print) comefrom) 
  (for/hash ([(footer-pc header-pc) (in-hash loop-footers)])
    (define (extend-path pc rest)
      (cond
        [(= pc header-pc)
         (list (cons pc rest))]
        [(and (not (= pc footer-pc))
              (hash-ref loop-footers pc #f)) 
         => (λ (header-pc)
              (let ([rest (cons (loop header-pc pc) rest)])
                (append-map
                 (λ (pred-pc) (extend-path pred-pc rest))
                 (filter
                  (λ (pred-pc) (subset? (hash-ref dominators pred-pc) (hash-ref dominators header-pc)))
                  (hash-ref comefrom header-pc)))))]
        [else
         (let ([rest (cons pc rest)])
           (append-map
            (λ (pred-pc) (extend-path pred-pc rest))
            (filter
             (λ (pred-pc) (subset? (hash-ref dominators header-pc) (hash-ref dominators pred-pc)))
             (hash-ref comefrom pc))))]))
    (values (loop header-pc footer-pc)
            (extend-path footer-pc null))))

(define (loops goto comefrom)
  (let* ([dominators (dominators goto comefrom)]
         [loop-footers (for*/hasheqv ([(pc dest-pcs) (in-hash goto)]
                                      [dest-pc (in-list dest-pcs)]
                                      #:when (set-member? (hash-ref dominators pc) dest-pc))
                         (values pc dest-pc))])
    (loops-as-paths dominators loop-footers comefrom)))

(define (loop-parents loops loop)
  (let ([blocks (hash-ref loops loop)])
    (for*/list ([(lp blks) (in-hash loops)]
                #:unless (equal? lp loop)
                #:when (subset? blocks blks))
      lp)))

(provide (struct-out loop)
         loops
         loop-parents)
