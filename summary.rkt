#lang racket/base
(require racket/match
         racket/set
         "data.rkt"
         "instruction.rkt"
         "symbolic-locals.rkt"
         "symbolic-stack.rkt"
         (submod "descriptor.rkt" stack-action))

(define (summaries m)
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
    (define (add-edge -> pc₀ pc₁)
      (hash-update -> pc₀ (λ (pcs) (cons pc₁ pcs)) null))
    (define (s₀ todo seen costs <-)
      (match todo
        [(cons pc todo)
         (if (set-member? seen pc)
           (s₀ todo seen costs <-)
           (let-values ([(more-todo simple-cost locals-summary stack-summary effects final)
                         (s₁ (hash-ref sequences pc) (make-locals max-locals) (make-stack max-stack) 0 null)])
             (s₀ (append more-todo todo)
                 (set-add seen pc)
                 (hash-set costs pc (list simple-cost locals-summary stack-summary effects final))
                 (for/fold ([<- <-]) ([dest-pc (in-list more-todo)])
                   (add-edge <- dest-pc pc)))))]
        [(list)
         (values costs <-)]))
    (define (s₁ block locals stack simple-cost effects)
      (define (s₂ pc instr not-done done)
        (match instr
          [(or (family #rx"^goto" _ offset)
               #;(family #rx"^jsr"  _ offset)
               )
           (let ([dest-pc (+ pc offset)])
             (done (list dest-pc) `(goto , dest-pc)))]
          [(family #rx"^if-icmp(eq|ge|gt|le|lt|ne)$" opcode offset succ-pc)
           (let ([dest-pc (+ pc offset)])
             (match-define (list v₀ v₁) (stack-pop!* stack 2))
             (done (list succ-pc dest-pc) `(branch (,opcode ,v₀ ,v₁) ,dest-pc ,succ-pc)))]
          [(family #rx"^if" opcode offset succ-pc)
           (let ([dest-pc (+ pc offset)])
             (define v (stack-pop! stack))
             (done (list succ-pc dest-pc) `(branch (,opcode ,v) ,dest-pc ,succ-pc)))]
          [`(lookupswitch ,default-offset ,pairs)
           (done (cons (+ pc default-offset)
                       (for/list ([pair (in-list pairs)])
                         (match-let ([(list _ offset) pair])
                           (+ pc offset))))
                 `(lookupswitch ,(+ pc default-offset)
                                ,(for/list ([pair (in-list pairs)])
                                   (match-let ([(list x offset) pair])
                                     (list x (+ pc offset))))))]
          [`(tableswitch ,default-offset ,hi ,lo ,offsets)
           (done (cons (+ pc default-offset) (map (λ (offset) (+ pc offset)) offsets))
                 `(tableswitch ,(+ pc default-offset) ,hi ,lo ,(map (λ (offset) (+ pc offset)) offsets)))]
          ['return
           (done null 'return)]
          [(family #rx".return$" opcode)
           (define v (stack-pop! stack))
           (done null `(return ,opcode ,v))]
          ['athrow
           (define objectref (stack-pop! stack))
           (done null `(athrow ,objectref))]
          [(family #rx"^(a|d|f|i|l)load$" opcode n)
           (stack-push! stack (locals-ref locals n))
           (not-done #f)]
          [`(iconst ,i)
           (stack-push! stack i)
           (not-done #f)]
          [`(dconst ,i)
           (stack-push! stack (exact->inexact i))
           (not-done #f)]
          [`(lconst ,l)
           (stack-push! stack l)
           (not-done #f)]
          #;
          [(family #rx"^(d|f|i|l)const$" opcode n)
           (list opcode n)]
          [(family #rx"^iinc$" opcode index constant)
           (let ([v (locals-ref locals index)])
             (if (integer? v)
               (locals-set! locals index (+ v constant))
               (locals-set! locals index `(+ ,v ,constant))))
           (not-done #f)]
          [(family #rx"^(a|d|f|i|l)store$" opcode n)
           (define v (stack-pop! stack))
           (locals-set! locals n v)
           (not-done #f)]
               #;
               [(family #rx"^if(eq|ge|gt|le|lt|ne|nonnull|null)" opcode offset)
                (list opcode offset)]
               #;
               [(family #rx"^if-acmp(eq|ne)$" opcode offset)
                (list opcode offset)]
          [`(anewarray ,type)
           (define count (stack-pop! stack))
           (stack-push! stack `(anewarray ,type ,count))
           (not-done #f)]
          [`(bipush ,i)
           (stack-push! stack i)
           (not-done #f)]
          [`(checkcast ,class)
           (define objectref (stack-pop! stack))
           (stack-push! stack `(checkcast ,objectref ,class))
           (not-done #f)]
          [`(ldc ,v)
           (stack-push! stack v)
           (not-done #f)]
          [`(ldc-w ,v)
           (stack-push! stack v)
           (not-done #f)]
          [`(ldc2-w ,v)
           (stack-push! stack v)
           (not-done #f)]
               #;
               [`(multianewarray ,index ,count)
                `(multianewarray ,(look-up-constant index constant-pool) ,count)]
          [`(new ,class)
           (stack-push! stack `(new ,class))
           (not-done #f)]
          [`(newarray ,type)
           (define count (stack-pop! stack))
           (stack-push! stack `(newarray ,type ,count))
           (not-done #f)]
          [`(sipush ,i)
           (stack-push! stack i)
           (not-done #f)]
               #;
               [`(wide-iinc ,index ,const)
                `(wide-iinc ,index ,const)]
          [`(instanceof ,class)
           (define objectref (stack-pop! stack))
           (stack-push! stack `(instanceof ,objectref ,class))
           (not-done #f)]
          [`(invokeinterface ,method ,count)
           (define objectref (stack-pop! stack))
           (define-values (arguments result)
             (match-let ([(methodref _ (name-and-type _ descriptor)) method])
               (perform-descriptor! descriptor stack)))
           (define token (gensym 'token))
           (match result
             ['void (void)]
             [`(field ,f) (stack-push! stack `(result ,token ,f))])
           (not-done `(invokeinterface ,objectref ,method ,arguments ,token))]
          [`(invokespecial ,method)
           (define objectref (stack-pop! stack))
           (define-values (arguments result)
             (match-let ([(methodref _ (name-and-type _ descriptor)) method])
               (perform-descriptor! descriptor stack)))
           (define token (gensym 'token))
           (match result
             ['void (void)]
             [`(field ,f) (stack-push! stack `(result ,token ,f))])
           (not-done `(invokespecial ,objectref ,method ,arguments ,token))]
          [`(invokestatic ,method)
           (define-values (arguments result)
             (match-let ([(methodref _ (name-and-type _ descriptor)) method])
               (perform-descriptor! descriptor stack)))
           (define token (gensym 'token))
           (match result
             ['void (void)]
             [`(field ,f) (stack-push! stack `(result ,token ,f))])
           (not-done `(invokestatic ,method ,arguments ,token))]
          [`(invokevirtual ,method)
           (define objectref (stack-pop! stack))
           (define-values (arguments result)
             (match-let ([(methodref _ (name-and-type _ descriptor)) method])
               (perform-descriptor! descriptor stack)))
           (define token (gensym 'token))
           (match result
             ['void (void)]
             [`(field ,f) (stack-push! stack `(result ,token ,f))])
           (not-done `(invokevirtual ,objectref ,method ,arguments ,token))]
          [`(getfield ,field)
           (define objectref (stack-pop! stack))
           (define token (gensym 'token))
           (stack-push! stack `(access-token ,token))
           (not-done `(getfield ,objectref ,field ,token))]
          [`(putfield ,field)
           (match-define (list objectref v) (stack-pop!* stack 2))
           (not-done `(putfield ,objectref ,field ,v))]
          [`(getstatic ,field)
           (define token (gensym 'token))
           (stack-push! stack `(access-token ,token))
           (not-done `(getstatic ,field ,token))]
          [`(putstatic ,field)
           (define v (stack-pop! stack))
           (not-done `(putstatic ,field ,v))]
          [(family #rx".aload$" opcode)
           (match-define (list arrayref index) (stack-pop!* stack 2))
           (define token (gensym 'token))
           (stack-push! stack `(access-token ,token))
           (not-done `(aload ,opcode ,arrayref ,index ,token))]
          [(family #rx".astore$" opcode)
           (match-define (list arrayref index value) (stack-pop!* stack 3))
           (not-done `(astore ,opcode ,arrayref ,index ,value))]
          ['aconst-null
           (stack-push! stack 'null)
           (not-done #f)]
          ['arraylength
           (define arrayref (stack-pop! stack))
           (stack-push! stack `(arraylength ,arrayref))
           (not-done #f)]
          ['dup
           (define v (stack-pop! stack))
           (stack-push! stack v v)
           (not-done #f)]
          ['dup2
           (match-define (list v₀ v₁) (stack-pop!* stack 2))
           (stack-push! stack v₀ v₁ v₀ v₁)
           (printf "POSSIBLY UNSOUND if ~v is a long or double\n" v₀)
           (not-done #f)]
          ['pop
           (stack-pop! stack)
           (not-done #f)]
          ['pop2
           (define v (stack-pop! stack))
           (stack-pop! stack)
           (printf "POSSIBLY UNSOUND if ~v is a long or double\n" v)
           (not-done #f)]
          ; unary operators
          [(and opcode (or 'd2l
                           'dneg
                           'i2b 'i2d 'i2l
                           'l2d 'l2i))
           (define v (stack-pop! stack))
           (stack-push! stack `(,opcode ,v))
           (not-done #f)]
          ; binary operators
          [(and opcode (or 'dcmpg 'dcmpl
                           'dadd 'ddiv 'dmul
                           'iadd 'iand 'idiv 'imul 'ior 'isub
                           'lcmp
                           'ladd 'lmul 'lsub))
           (match-define (list v₀ v₁) (stack-pop!* stack 2))
           (stack-push! stack `(,opcode ,v₀ ,v₁))
           (not-done #f)]
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
          ))
      (match block
        [(cons (instruction pc instr) block)
         (if (and (set-member? jump-destinations pc) (not (zero? simple-cost)))
           (values (list pc)
                   simple-cost
                   (locals-summary locals)
                   (make-stack-summary stack)
                   effects
                   `(goto ,pc))
           (let ([simple-cost (add1 simple-cost)])
             (s₂ pc instr
                 (λ (e) (s₁ block locals stack simple-cost (if e (cons e effects) effects)))
                 (λ (more-todo final)
                   (values more-todo
                           simple-cost
                           (locals-summary locals)
                           (make-stack-summary stack)
                           effects
                           final)))))]))

    (define (reduce costs <-)
      (let loop ([todo (list 0)]
                 [h (hasheqv)])
        (match todo
          [(list) h]
          [(cons pc todo)
           (if (hash-has-key? h pc)
             (loop todo h)
             (let loop₂ ([summary (hash-ref costs pc)])
               (match-let ([(list simple-cost locals-summary stack-summary effects final) summary])
                 (match final
                   [`(goto ,dest-pc)
                    (if (null? (remv pc (hash-ref <- dest-pc)))
                      (match-let ([(list simple-cost₁ locals-summary₁ stack-summary₁ effects₁ final)  (hash-ref costs dest-pc)])
                        (displayln "MERGING!")
                        (loop₂ (list (+ simple-cost₁ simple-cost)
                                     (locals-summary-sequence locals-summary₁ locals-summary)
                                     (stack-summary-sequence stack-summary₁ stack-summary)
                                     (append effects₁ effects)
                                     final)))
                      (loop (cons dest-pc todo) (hash-set h pc summary)))]
                   ['return
                    (loop todo (hash-set h pc summary))]
                   [`(return . ,_)
                    (loop todo (hash-set h pc summary))]
                   [`(athrow . ,_)
                    (loop todo (hash-set h pc summary))]
                   [`(branch ,_ ,jump-pc ,succ-pc)
                    (loop (list* jump-pc succ-pc todo) (hash-set h pc summary))]
                   [`(lookupswitch ,default-pc ,pairs)
                    (loop (for/fold ([todo (cons default-pc todo)])
                                    ([pair (in-list pairs)])
                            (match-let ([(list _ dest-pc) pair])
                              (cons dest-pc todo)))
                          (hash-set h pc summary))]
                   [`(tableswitch ,default-pc ,_ ,_ ,dest-pcs)
                    (loop (for/fold ([todo (cons default-pc todo)])
                                    ([dest-pc (in-list dest-pcs)])
                            (cons dest-pc todo))
                          (hash-set h pc summary))]))))])))

    (call-with-values (λ () (s₀ (list 0) (seteqv) (hasheqv) (hasheqv))) reduce)
    
    #;
    (define (reduce h)
      
      #;
      (for/fold ([h (hasheqv)]
                 [fold]))
      #;
      h
      #;
      (for/hasheqv ([(pc summary) (in-hash h)])
        (match-let ([(list simple-cost (cons r es)) summary])
          (values pc `(+ ,simple-cost ,r . ,(let loop ([es es])
                                           (match es
                                             [(cons e es)
                                              (match e
                                                [(or `(invokespecial ,_ ,method ,_ ,_)
                                                     `(invokestatic ,method ,_ ,_)
                                                     `(invokevirtual ,_ ,method ,_ ,_)
                                                     `(invokeinterface ,_ ,method ,_ ,_))
                                                 (cons (equal-hash-code e) (loop es))]
                                                [(or `(getstatic . ,_)
                                                     `(putstatic . ,_)
                                                     `(getfield . ,_)
                                                     `(putfield . ,_)
                                                     `(aastore . ,_)
                                                     `(bastore . ,_)
                                                     `(iaload . ,_)
                                                     `(iastore . ,_)
                                                     `(aaload . ,_)
                                                     `(baload . ,_))
                                                 (loop es)])]
                                             [(list) (list)]))
                         ))))
      #;
      (let-values ([(key value) (hash-iterate-key+value h (hash-iterate-first h))])
        (match value))
      
      ))
  (costs bytecode))
    
    



(provide summaries)


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
