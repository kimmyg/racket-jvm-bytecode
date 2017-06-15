#lang racket/base
(require racket/match
         racket/set
         "data.rkt"
         "instruction.rkt")

(define (loops m)
  (define (jumps instrs)
    ((current-print) instrs)
    (define jump-destinations
      (for/fold ([jds (seteqv)]) ([instr (in-list instrs)])
        (match instr
          [(instruction pc instr)
           (match instr
             [(or (family #rx"^goto" _ offset)
                  (family #rx"^jsr" _ offset)
                  (family #rx"^if" _ offset))
              (set-add jds (+ pc offset))]
             [`(lookupswitch ,default-offset ,pairs)
              (for/fold ([jds (set-add jds (+ pc default-offset))]) ([pair (in-list pairs)])
                (match-let ([(list _ offset) pair])
                  (set-add jds (+ pc offset))))]
             [`(tableswitch ,default-offset ,_ ,_ ,offsets)
              (for/fold ([jds (set-add jds (+ pc default-offset))]) ([offset (in-list offsets)])
                (set-add jds (+ pc offset)))]
             [_ jds])])))
    (define sequences
      (let loop ([sequences (hasheqv)]
                 [instrs instrs])
        (match instrs
          [(cons (instruction pc _) instrs*)
           (loop (hash-set sequences pc instrs)
                 instrs*)]
          [(list)
           sequences])))
    (define (add-edge -> pc₀ pc₁)
      (hash-update -> pc₀ (λ (pcs) (cons pc₁ pcs)) null))
    (define (s₀ todo seen ->)
      (match todo
        [(cons pc todo)
         (if (set-member? seen pc)
           (s₀ todo seen ->)
           (s₁ todo seen -> pc (hash-ref sequences pc)))]
        [(list)
         ->]))
    (define (s₁ todo seen -> block-pc block)
      (match block
        [(cons (instruction pc instr) block)
         (let ([seen (set-add seen pc)])
           (match instr
             [(or (family #rx"^goto" _ offset)
                  (family #rx"^jsr"  _ offset))
              (let ([dest-pc (+ pc offset)])
                (s₀ (cons dest-pc todo) seen (add-edge -> block-pc dest-pc)))]
             [(family #rx"^if" _ offset)
              (let*-values ([(todo ->) (match-let ([(cons (instruction pc _) _) block])
                                         (values (cons pc todo)
                                                 (add-edge -> block-pc pc)))]
                            [(todo ->) (let ([dest-pc (+ pc offset)])
                                         (values (cons dest-pc todo)
                                                 (add-edge -> block-pc dest-pc)))])
                (s₀ todo seen ->))]
             [`(lookupswitch ,default-offset ,pairs)
              (let-values ([(todo ->) (for/fold ([todo todo] [-> ->])
                                                ([pair (in-list pairs)])
                                        (match-let ([(list _ offset) pair])
                                          (let ([dest-pc (+ pc offset)])
                                            (values (cons dest-pc todo)
                                                    (add-edge -> block-pc dest-pc)))))])
                (s₀ todo seen ->))]
             [`(tableswitch ,default-offset ,_ ,_ ,offsets)
              (let*-values ([(todo ->) (let ([dest-pc (+ pc default-offset)])
                                         (values (cons dest-pc todo) (add-edge -> block-pc dest-pc)))]
                            [(todo ->) (for/fold ([todo todo] [-> ->])
                                                 ([offset (in-list offsets)])
                                         (let ([dest-pc (+ pc offset)])
                                           (values (cons dest-pc todo) (add-edge -> block-pc dest-pc))))])
                (s₀ todo seen ->))]
             [(family #rx"return$" _)
              (s₀ todo seen ->)]
             [(family #rx"throw$" _)
              (s₀ todo seen ->)]
             [_
              (if (and (set-member? jump-destinations pc) (not (= pc block-pc)))
                (s₁ todo seen (add-edge -> block-pc pc) pc block)
                (s₁ todo seen -> block-pc block))]))]))
    (s₀ (list 0) (seteqv) (hasheqv)))
  (values (jumps (code-bytecode (cdr (assq 'Code (jvm-method-attributes m)))))))

(provide loops)


#|

  #;
  (define block-entry-pc
    (match-lambda
      [(cons (instruction pc _) _) pc]))
  #;
  (define (block-exit-target-pcs blk succ-blk-pc) 
    (match blk
      [(list _ ... (instruction pc instr))
       (match instr
         [(family #rx"^if" _ offset)
          (if succ-blk-pc
            (list (+ pc offset) succ-blk-pc)
            (error 'block-exit-target-pcs "expected a successor block PC"))]
         [(family #rx"^goto" _ offset)
          (list (+ pc offset))]
         [(family #rx"^jsr" _ offset)
          (list (+ pc offset))]
         [(family #rx"return$" _)
          null]
         ['athrow
          null]
         [instr
          (if succ-blk-pc
            (list succ-blk-pc)
            (error 'block-exit-target-pcs "expected a successor block PC for instruction ~v" instr))])]))
  #;
  (define bs (blocks (code-bytecode (cdr (assq 'Code (jvm-method-attributes m))))))
  #;
  ((current-print) bs)
  #;
  (let loop ([bs bs]
             [-> (hasheqv)]
             [<- (hasheqv)])
    (match bs
      [(cons b₀ (and bs₀ (cons b₁ bs₁)))
       (let ([entry-pc (block-entry-pc b₀)]
             [exit-pcs (block-exit-target-pcs b₀ (block-entry-pc b₁))])
         (loop bs₀
               (for/fold ([-> ->]) ([exit-pc (in-list exit-pcs)])
                 (hash-update -> entry-pc (λ (pcs) (cons exit-pc pcs)) null))
               (for/fold ([<- <-]) ([exit-pc (in-list exit-pcs)])
                 (hash-update <- exit-pc (λ (pcs) (cons entry-pc pcs)) null))))]
      [(list b₀)
       (let ([entry-pc (block-entry-pc b₀)]
             [exit-pcs (block-exit-target-pcs b₀ #f)])
         (list (for/fold ([-> ->]) ([exit-pc (in-list exit-pcs)])
                 (hash-update -> entry-pc (λ (pcs) (cons exit-pc pcs)) null))
               (for/fold ([<- <-]) ([exit-pc (in-list exit-pcs)])
                 (hash-update <- exit-pc (λ (pcs) (cons entry-pc pcs)) null))))]))









    #;
    (let loop ([-> (hasheqv)]
               [instrs instrs]
               [block-pc])
      (match-let ([(cons instr instrs) instrs])
        (match instrs
          [(cons (instruction pc _) _)
           (loop (link -> instr₀ pc) instrs)]
          [(list)
           (link -> instr₀ #f)])))

    #;
    (define (block-boundaries instrs)
      (let loop ([boundaries (seteqv)]
                 [instrs instrs])
        (match instrs
          [(cons (instruction pc₀ instr) instrs)
           (match instr
             [(or (family #rx"^if" _ offset)
                  (family #rx"^goto" _ offset)
                  (family #rx"^jsr" _ offset))
              (loop (set-add (match instrs
                               [(cons (instruction pc₁ _) _)
                                (set-add boundaries pc₁)]
                               [_
                                boundaries])
                             (+ pc₀ offset))
                    instrs)]
             []
             [_
              (loop boundaries instrs)])]
          [(list)
           boundaries]))
      #;
      (define (pred prev next dest-pc)
        (match next
          [(cons (instruction (== dest-pc) _) _) next]
          [_ (pred (cdr prev) (cons (car prev) next) dest-pc)]))
      #;
      (define (succ prev next dest-pc)
        (match next
          [(cons (instruction (== dest-pc) _) _) next]
          [_ (succ (cons (car next) prev) (cdr next) dest-pc)]))
      #;
      (let loop ([boundaries (set)]
                 [prev null]
                 [next instrs])
        (match next
          [(cons (and instr (instruction pc instr*)) next*)
           (loop (match instr*
                   [(or (family #rx"^if" _ offset)
                        (family #rx"^goto" _ offset)
                        (family #rx"^jsr" _ offset))
                    (set-add (set-add boundaries next*)
                             (if (< offset 0)
                               (pred prev next (+ pc offset))
                               (succ prev next (+ pc offset))))]
                   [`(lookupswitch . ,rst)
                    (raise `(lookupswitch . ,rst))]
                   [`(table . ,rst)
                    (raise `(tableswitch . ,rst))]
                   [_ boundaries])
                 (cons instr prev) next*)]
          [(list) boundaries])))
    #;
    (define (block-boundaries instrs)
      (define (pred prev next dest-pc)
        (match next
          [(cons (instruction (== dest-pc) _) _) next]
          [_ (pred (cdr prev) (cons (car prev) next) dest-pc)]))
      (define (succ prev next dest-pc)
        (match next
          [(cons (instruction (== dest-pc) _) _) next]
          [_ (succ (cons (car next) prev) (cdr next) dest-pc)]))
      (let loop ([boundaries (set)]
                 [prev null]
                 [next instrs])
        (match next
          [(cons (and instr (instruction pc instr*)) next*)
           (loop (match instr*
                   [(or (family #rx"^if" _ offset)
                        (family #rx"^goto" _ offset)
                        (family #rx"^jsr" _ offset))
                    (set-add (set-add boundaries next*)
                             (if (< offset 0)
                               (pred prev next (+ pc offset))
                               (succ prev next (+ pc offset))))]
                   [`(lookupswitch . ,rst)
                    (raise `(lookupswitch . ,rst))]
                   [`(table . ,rst)
                    (raise `(tableswitch . ,rst))]
                   [_ boundaries])
                 (cons instr prev) next*)]
          [(list) boundaries])))
    #;
    (let ([bs (block-boundaries instrs)])
      (let loop ([block null]
                 [instrs instrs])
        (match instrs
          [(cons (and instr (instruction pc _)) instrs)
           (if (set-member? bs pc)
             (if (null? block)
               (loop (cons instr block) instrs)
               (cons (reverse block) (loop (list instr) instrs)))
             (loop (cons instr block) instrs))]
          [(list)
           (list (reverse block))])))

    #;
    #;
    (define (link₀ -> block-pc instr-pc instr succ-instr-pc instrs)
      (match instr
        [(or (family #rx"^if" _ offset)
             (family #rx"^goto" _ offset)
             (family #rx"^jsr" _ offset))
         (let* ([-> (if succ-instr-pc (add-edge -> block-pc succ-instr-pc) ->)]
                [-> (add-edge -> block-pc (+ instr-pc offset))])
           (link₁ -> succ-instr-pc instrs))]
        [_
         (link₁ -> block-pc instrs)]))
    #;
    (define (link₁ -> block-pc instrs)
      (match instrs
        [(cons (instruction pc₀ instr₀) instrs)
         (match instrs
           [(cons (instruction pc₁ _) _)
            (link₀ -> block-pc pc₀ instr₀ pc₁ instrs)]
           [(list)
            (link₀ -> block-pc pc₀ instr₀ #f instrs)])]
        [(list)
         ->]))
|#