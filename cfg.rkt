#lang racket/base
(require racket/list
         racket/set)

(define (add-edge -> pc₀ pc₁)
  (hash-update -> pc₀ (λ (pcs) (cons pc₁ pcs)) null))

(define (cfg-reverse ->)
  (for*/fold ([<- (hasheqv)]) ([(pc dest-pcs) (in-hash ->)]
                               [dest-pc (in-list dest-pcs)])
    (add-edge <- dest-pc pc)))

(define (blocks goto)
  (set-add (apply seteqv (apply append (hash-values goto))) 0))

(define (dominators goto comefrom)
  (let* ([blocks (blocks goto)]
         [non-root-blocks (set-remove blocks 0)])
    (define dominators
      (let ([dominators (for/hasheqv ([n (in-set non-root-blocks)])
                          (values n blocks))])
        (hash-set dominators 0 (seteqv 0))))
    (let loop ([dominators dominators])
      (let-values ([(dominators changed?)
                    (for/fold ([dominators dominators]
                               [changed? #f])
                              ([n (in-set non-root-blocks)])
                      (let ([d (set-add (for/fold ([d blocks]) ([pred-n (in-list (hash-ref comefrom n))])
                                          (set-intersect d (hash-ref dominators pred-n)))
                                        n)])
                        (values (hash-set dominators n d) (or changed? (not (equal? d (hash-ref dominators n)))))))])
        (if changed? (loop dominators) dominators)))))

(define (loop-headers goto dominators)
  (for*/fold ([headers (hasheqv)]) ([(pc dest-pcs) (in-hash goto)]
                                    [dest-pc (in-list dest-pcs)]
                                    #:when (set-member? (hash-ref dominators pc) dest-pc))
    (add-edge headers dest-pc pc)))

(define (loop-footers goto dominators)
  (for*/hasheqv ([(pc dest-pcs) (in-hash goto)]
                 [dest-pc (in-list dest-pcs)]
                 #:when (set-member? (hash-ref dominators pc) dest-pc))
    (values pc dest-pc)))

(struct path (header-block-pc footer-block-pc) #:transparent)
(struct loop path () #:transparent)

#;
(define (paths goto dominators)
  ((current-print) goto)
  ((current-print) dominators)
  (let* ([loop-headers (loop-headers goto dominators)]
         [_ ((current-print) loop-headers)]
         [loop-paths (for*/hash ([(header-pc footer-pcs) (in-hash loop-headers)]
                                 [footer-pc (in-list footer-pcs)])
                       (define (extend-path pc rest)
                         ((current-print) (cons pc rest))
                         (when (< (random) 0.01) (raise 'stop))
                         (cond
                           [(= pc footer-pc)
                            (list (reverse (cons pc rest)))]
                           [(hash-ref loop-headers pc #f)
                            => (λ (footer-pcs)
                                 (append-map
                                  (λ (footer-pc)
                                    (let ([rest (cons (loop pc footer-pc) rest)])
                                      (append-map
                                       (λ (succ-pc) (extend-path succ-pc rest))
                                       (filter
                                        (λ (succ-pc) (subset? (hash-ref dominators succ-pc) (hash-ref dominators footer-pc)))
                                        (hash-ref goto footer-pc)))))
                                  footer-pcs))]
                           [else
                            (let ([rest (cons pc rest)])
                              (append-map
                               (λ (succ-pc) (extend-path succ-pc rest))
                               (filter
                                (λ (succ-pc) (subset? (hash-ref dominators pc) (hash-ref dominators succ-pc)))
                                (hash-ref goto pc))))]))
                       (values (loop header-pc footer-pc)
                               (extend-path header-pc null)))])
    
    loop-paths))


(define (paths goto comefrom dominators)
  (let ([loop-footers (loop-footers goto dominators)])
    (define (paths-from header-pc footer-pc)
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
      (extend-path footer-pc null))
    ; this function is necessary since not all loops (cycles in the CFG) are detected by Soot's loop-finding method
    (define (paths-from-hack header-pc footer-pc)
      (define (extend-path pc rest seen)
        (cond
          [(set-member? seen pc)
           (list (cons `(cycle ,pc) rest))]
          [(= pc header-pc)
           (list (cons pc rest))]
          [(and (not (= pc footer-pc))
                (hash-ref loop-footers pc #f)) 
           => (λ (header-pc)
                (let ([rest (cons (loop header-pc pc) rest)])
                  (append-map
                   (λ (pred-pc) (extend-path pred-pc rest (set-add seen pc)))
                   (filter
                    (λ (pred-pc) (subset? (hash-ref dominators pred-pc) (hash-ref dominators header-pc)))
                    (hash-ref comefrom header-pc)))))]
          [else
           (let ([rest (cons pc rest)])
             (append-map
              (λ (pred-pc) (extend-path pred-pc rest (set-add seen pc)))
              (filter
               (λ (pred-pc) (subset? (hash-ref dominators header-pc) (hash-ref dominators pred-pc)))
               (hash-ref comefrom pc))))]))
      (extend-path footer-pc null (seteqv)))
    (let ([loop-paths (for/hash ([(footer-pc header-pc) (in-hash loop-footers)])
                        (values (loop header-pc footer-pc)
                                (paths-from header-pc footer-pc)))]
          [method-exits (for*/seteqv ([(_ dest-pcs) (in-hash goto)]
                                       [dest-pc (in-list dest-pcs)]
                                      #:unless (hash-has-key? goto dest-pc))
                                     dest-pc)])
      (if (hash-empty? goto)
        (hash-set loop-paths (path 0 0) '((0)))
        (for*/fold ([paths loop-paths]) ([exit-pc (in-set method-exits)])
          (hash-set paths
                    (path 0 exit-pc)
                    (paths-from-hack 0 exit-pc)))))))

(provide (all-defined-out))
