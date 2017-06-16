#lang racket/base
(require racket/match
         racket/set
         "data.rkt"
         "instruction.rkt"
         (submod "descriptor.rkt" stack-action))

(struct indeterminate-stack (map ))

(define (loops m)
  (match-define (code max-stack max-locals bytecode exception-table attributes)
    (cdr (assq 'Code (jvm-method-attributes m))))
  (define (costs instrs)
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
    (define (s₀ todo seen costs)
      (match todo
        [(cons pc todo)
         (if (set-member? seen pc)
           (s₀ todo seen costs)
           (let-values ([(more-todo simple-cost etc) (s₁ (hash-ref sequences pc) (apply vector (build-list max-locals (λ (i) `(local ,i)))) null 0 null)])
             (s₀ (append more-todo todo) (set-add seen pc) (hash-set costs pc (list simple-cost etc)))))]
        [(list)
         costs]))
    (define (s₁ block locals stack simple-cost etc)
      (let ([simple-cost (add1 simple-cost)])
        (match block
          [(cons (instruction pc instr) block)
           (if (and (set-member? jump-destinations pc) (not (= simple-cost 1)))
             (values (list pc) simple-cost (cons pc etc))
             (match instr
               [(or (family #rx"^goto" _ offset)
                    (family #rx"^jsr"  _ offset))
                (let ([dest-pc (+ pc offset)])
                  (values (list dest-pc) simple-cost (cons dest-pc etc)))]
               [(family #rx"^if" opcode offset)
                (let ([dest-pc (+ pc offset)]
                      [succ-pc (match-let ([(cons (instruction pc _) _) block]) pc)])
                  (match-let ([(cons v stack) stack])
                    (values (list succ-pc dest-pc) simple-cost (cons `(if (,opcode ,v) ,dest-pc ,succ-pc) etc))))]
               [`(lookupswitch ,default-offset ,pairs)
                (values (cons (+ pc default-offset)
                              (for/list ([pair (in-list pairs)])
                                (match-let ([(list _ offset) pair])
                                  (+ pc offset))))
                        simple-cost
                        (cons `(lookupswitch ,(+ pc default-offset)
                                             ,(for/list ([pair (in-list pairs)])
                                                (match-let ([(list x offset) pair])
                                                  (list x (+ pc offset)))))
                              etc))]
               [`(tableswitch ,default-offset ,hi ,lo ,offsets)
                (values (cons (+ pc default-offset) (map (λ (offset) (+ pc offset)) offsets))
                        simple-cost
                        (cons `(tableswitch ,(+ pc default-offset) ,hi ,lo ,(map (λ (offset) (+ pc offset)) offsets)) etc))]
               ['return
                (values null simple-cost (cons 'return etc))]
               [(family #rx".return$" opcode)
                (match stack
                  [(cons v stack)
                   (values null simple-cost (cons `(,opcode ,v) etc))]
                  [(list)
                   (values null simple-cost (cons `(,opcode (stack -1)) etc))])]
               ['athrow
                (match-let ([(cons objectref stack) stack])
                  (values null simple-cost (cons `(athrow ,objectref) etc)))]
               [(family #rx"^(a|d|f|i|l)load$" opcode n)
                (let ([stack (cons (vector-ref locals n) stack)])
                  (s₁ block locals stack simple-cost etc))]
               [`(iconst ,i)
                (let ([stack (cons i stack)])
                  (s₁ block locals stack simple-cost etc))]
               [`(dconst ,i)
                (let ([stack (cons (exact->inexact i) stack)])
                  (s₁ block locals stack simple-cost etc))]
               [`(lconst ,l)
                (let ([stack (cons l stack)])
                  (s₁ block locals stack simple-cost etc))]
               #;
               [(family #rx"^(d|f|i|l)const$" opcode n)
                (list opcode n)]
               [(family #rx"^iinc$" opcode index constant)
                (let ([v (vector-ref locals index)])
                  (if (integer? v)
                    (vector-set! locals index (+ v constant))
                    (vector-set! locals index `(+ ,v ,constant))))
                (s₁ block locals stack simple-cost etc)]
               [(family #rx"^(a|d|f|i|l)store$" opcode n)
                (match-let ([(cons v stack) stack])
                  (vector-set! locals n v)
                  (s₁ block locals stack simple-cost etc))]
               #;
               [(family #rx"^if(eq|ge|gt|le|lt|ne|nonnull|null)" opcode offset)
                (list opcode offset)]
               #;
               [(family #rx"^if-acmp(eq|ne)$" opcode offset)
                (list opcode offset)]
               #;
               [(family #rx"^if-icmp(eq|ge|gt|le|lt|ne)$" opcode offset)
                (list opcode offset)]
               #;
               [`(anewarray ,type)
                (match-let ([(cons count stack) stack])
                  (let ([stack (cons `(a))]))
                  )
                `(anewarray ,(look-up-constant index constant-pool))]
               [`(bipush ,i)
                (let ([stack (cons i stack)])
                  (s₁ block locals stack simple-cost etc))]
               [`(checkcast ,class)
                (match-let* ([(cons objectref stack) stack]
                             [stack (cons `(checkcast ,objectref ,class) stack)])
                  (s₁ block locals stack simple-cost etc))]
               #;
               [`(goto ,offset)
                `(goto ,offset)]
               [`(ldc ,v)
                (let ([stack (cons v stack)])
                  (s₁ block locals stack simple-cost etc))]
               #;
               [`(ldc-w ,index)
                `(ldc-w ,(look-up-constant index constant-pool))]
               [`(ldc2-w ,v)
                (let ([stack (cons v stack)])
                  (s₁ block locals stack simple-cost etc))]
               #;
               [`(lookupswitch ,de ,offsets)
                `(lookupswitch ,de ,offsets)]
               #;
               [`(jsr ,offset)
                `(jsr ,offset)]
               #;
               [`(multianewarray ,index ,count)
                `(multianewarray ,(look-up-constant index constant-pool) ,count)]
               [`(new ,class)
                (let ([stack (cons `(new ,class) stack)])
                  (s₁ block locals stack simple-cost etc))]
               [`(newarray ,type)
                (match-let ([(cons count stack) stack])
                  (let ([stack (cons `(newarray ,type ,count) stack)])
                    (s₁ block locals stack simple-cost etc)))]
               #;
               [`(ret ,offset)
                `(ret ,offset)]
               [`(sipush ,i)
                (let ([stack (cons i stack)])
                  (s₁ block locals stack simple-cost etc))]
               #;
               [`(tableswitch ,de ,lo ,hi ,offsets)
                `(tableswitch ,de ,lo ,hi ,offsets)]
               #;
               [`(wide-iinc ,index ,const)
                `(wide-iinc ,index ,const)]
               #;
               [`(instanceof ,index)
                `(instanceof ,(look-up-constant index constant-pool))]
               #;
               [`(invokeinterface ,index ,count)
                `(invokeinterface ,(look-up-constant index constant-pool) ,count)]
               [`(invokeinterface ,method ,count)
                (match-let ([(cons objectref stack) stack])
                  (let-values ([(arguments stack) (match-let ([(methodref _ (name-and-type _ descriptor)) method])
                                                    (perform-descriptor descriptor stack))])
                    (s₁ block locals stack simple-cost (cons `(invokeinterface ,objectref ,method ,arguments) etc))))]
               [`(invokespecial ,method)
                (match-let ([(cons objectref stack) stack])
                  (let-values ([(arguments stack) (match-let ([(methodref _ (name-and-type _ descriptor)) method])
                                                    (perform-descriptor descriptor stack))])
                    (s₁ block locals stack simple-cost (cons `(invokespecial ,objectref ,method ,arguments) etc))))]
               [`(invokestatic ,method)
                (let-values ([(arguments stack) (match-let ([(methodref _ (name-and-type _ descriptor)) method])
                                                  (perform-descriptor descriptor stack))])
                  (s₁ block locals stack simple-cost (cons `(invokestatic ,method ,arguments) etc)))]
               [`(invokevirtual ,method)
                (match-let ([(cons objectref stack) stack])
                  (let-values ([(arguments stack) (match-let ([(methodref _ (name-and-type _ descriptor)) method])
                                                    (perform-descriptor descriptor stack))])
                    (s₁ block locals stack simple-cost (cons `(invokevirtual ,objectref ,method ,arguments) etc))))]
               [`(getfield ,field)
                (match-let* ([(cons objectref stack) stack]
                             [stack (cons `(getfield ,objectref ,field) stack)])
                  (displayln "getting an instance field; should keep track of gets and puts")
                  (s₁ block locals stack simple-cost etc))]
               [`(putfield ,field)
                (match-let ([(list* objectref v stack) stack])
                  (displayln "putting an instance field; should keep track of gets and puts")
                  (s₁ block locals stack simple-cost etc))]
               #;
               [`(putfield ,index)
                `(putfield ,(look-up-constant index constant-pool))]
               [`(getstatic ,field)
                (let ([stack (cons `(getstatic ,field) stack)])
                  (printf "UNSOUND: need to track putstatic\n")
                  (s₁ block locals stack simple-cost etc))]
               [`(putstatic ,field)
                (match-let ([(cons v stack) stack])
                  (printf "EFFECT: ~a\n" `(putstatic ,field ,v))
                  (s₁ block locals stack simple-cost etc))]
               [(family #rx".aload$" opcode)
                (match-let ([(list* arrayref index stack) stack])
                  (let ([stack (cons `(,opcode ,arrayref ,index) stack)])
                    (s₁ block locals stack simple-cost etc)))]
               ['aconst-null
                (let ([stack (cons 'null stack)])
                  (s₁ block locals stack simple-cost etc))]
               ['arraylength
                (match-let ([(cons arrayref stack) stack])
                  (let ([stack (cons `(arraylength ,arrayref) stack)])
                    (s₁ block locals stack simple-cost etc)))]
               ['dup
                (match-let* ([(cons v stack) stack]
                             [stack (list* v v stack)])
                  (s₁ block locals stack simple-cost etc))]
               ['dup2
                (match-let ([(list* v₀ v₁ stack) stack])
                  (printf "POSSIBLY UNSOUND if ~v is a long or double\n" v₀)
                  (let ([stack (list* v₀ v₁ v₀ v₁ stack)])
                    (s₁ block locals stack simple-cost etc)))]
               ['iastore
                (match-let ([(list* arrayref index value stack) stack])
                  (printf "EFFECT: ~a\n" `(iastore ,arrayref ,index ,value))
                  (s₁ block locals stack simple-cost etc))]
               ['pop
                (match-let ([(cons v stack) stack])
                  (s₁ block locals stack simple-cost etc))]
               ; unary operators
               [(and opcode (or 'd2l
                                'dneg
                                'i2d 'i2l
                                'l2d))
                (match-let* ([(list* v stack) stack]
                             [stack (cons `(,opcode ,v) stack)])
                  (s₁ block locals stack simple-cost etc))]
               ; binary operators
               [(and opcode (or 'dcmpg 'dcmpl
                                'dadd 'ddiv 'dmul
                                'iadd 'iand 'idiv 'imul 'isub
                                'ladd 'lmul))
                (match-let* ([(list* v₀ v₁ stack) stack]
                             [stack (cons `(,opcode ,v₀ ,v₁) stack)])
                  (s₁ block locals stack simple-cost etc))]
               #;
               [(and opcode (or 'aaload 'aastore 'aconst-null 'areturn 'arraylength 'athrow
                                'baload 'bastore
                                'caload 'castore
                                'd2f 'd2i 'd2l 'dadd 'daload 'dastore 'dcmpg 'dcmpl 'ddiv 'dmul 'dneg 'dreturn 'dsub 'dup 'dup2 'dup2-x1 'dup-x1 'dup-x2
                                'f2d 'f2i 'f2l 'fadd 'faload 'fastore 'fcmpg 'fcmpl 'fdiv 'fmul 'freturn 'fsub
                                'i2b 'i2c 'i2d 'i2f 'i2l 'i2s 'iadd 'iaload 'iand 'iastore 'idiv 'imul 'ineg 'ior 'irem 'ireturn 'ishl 'ishr 'isub 'iushr 'ixor
                                'l2d 'l2f 'l2i 'ladd 'land 'laload 'lastore 'lcmp 'ldiv 'lmul 'lneg 'lor 'lrem 'lreturn 'lshl 'lshr 'lsub 'lushr 'lxor
                                'monitorenter 'monitorexit
                                'pop 'pop2
                                'return
                                'saload 'sastore))
                opcode]
               ))])))
    (s₀ (list 0) (seteqv) (hasheqv)))
  (costs bytecode))
    
    



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
