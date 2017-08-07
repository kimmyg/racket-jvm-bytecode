#lang racket/base
(require racket/match
         racket/set
         "data.rkt"
         "cfg.rkt"
         "instruction.rkt"
         "symbolic-locals.rkt"
         "symbolic-stack.rkt"
         (submod "descriptor.rkt" stack-action))



(struct summary (simple-cost locals-summary stack-summary effects final) #:transparent)

(define (summarize m)
  (match-define (code max-stack max-locals bytecode exception-table attributes)
    (cdr (assq 'Code (jvm-method-attributes m))))
  (define (summaries instrs)
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
    (define (s₀ todo seen costs ->)
      (match todo
        [(cons pc todo)
         (if (set-member? seen pc)
           (s₀ todo seen costs ->)
           (let-values ([(more-todo simple-cost locals-summary stack-summary effects final)
                         (s₁ (hash-ref sequences pc) (make-locals max-locals) (make-stack max-stack) 0 null)])
             (s₀ (append more-todo todo)
                 (set-add seen pc)
                 (hash-set costs pc (summary simple-cost locals-summary stack-summary effects final))
                 (for/fold ([-> ->]) ([dest-pc (in-list more-todo)])
                   (add-edge -> pc dest-pc)))))]
        [(list)
         (values costs -> (cfg-reverse ->))]))
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
               (locals-set! locals index `(binop ,opcode ,v ,constant))))
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
          ['dup-x1
           (match-define (list v₀ v₁) (stack-pop!* stack 2))
           (stack-push! stack v₀ v₁ v₀)
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
                           'i2b 'i2d 'i2l 'i2s
                           'l2d 'l2i))
           (define v (stack-pop! stack))
           (stack-push! stack `(unop ,opcode ,v))
           (not-done #f)]
          ; binary operators
          [(and opcode (or 'dcmpg 'dcmpl
                           'dadd 'ddiv 'dmul
                           'iadd 'iand 'idiv 'imul 'ineg 'ior 'isub 'ishl 'ishr 'iushr 'ixor
                           'lcmp
                           'land 'ladd 'lmul 'lor 'lsub 'lshl 'lshr 'lxor))
           (match-define (list v₀ v₁) (stack-pop!* stack 2))
           (stack-push! stack `(binop ,opcode ,v₀ ,v₁))
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
    (s₀ (list 0) (seteqv) (hasheqv) (hasheqv)))
  (summaries bytecode))

(define (sequence-summary pcs summaries)
  (define (collapse-adjacent-summaries s₀ pc₁ s₁)
    (match* (s₀ s₁)
      [((summary sc₀ ls₀ ss₀ e₀ f₀)
        (summary sc₁ ls₁ ss₁ e₁ f₁))
       (summary (+ sc₀ sc₁)
                #f
                (collapse-adjacent-stack-summaries ss₀ ss₁)
                (append e₀ e₁)
                f₁
                #;
                (match f₀
                  [`(branch ,condition ,jump-pc ,succ-pc)
                   (cond
                     [(= pc₁ jump-pc)
                      `(pos ,condition)]
                     [(= pc₁ succ-pc)
                      `(neg ,condition)]
                     [else
                      (error 'sequence-summary "bad jump!")])]))
       
       ]))
  (define (sequence-summary* s pcs)
    (match pcs
      [(list) s]
      [(cons pc pcs)
       (sequence-summary* (collapse-adjacent-summaries s pc (hash-ref summaries pc)) pcs)]))
  (match pcs
    [(list) (error 'sequence-summary "expected a non-empty sequence")]
    [(cons pc pcs) (sequence-summary* (hash-ref summaries pc) pcs)]))

#;
(define (path-summary path summaries)
  (let ([summaries])))

(provide (struct-out summary) summarize sequence-summary)

#|
(define (resolve-operand effects operand)
  (match operand
    [`(result ,token ,_)
     (findf
      (match-lambda
        [(list _ ... (== token)) #t]
        [_ #f])
      (effect-calls effects))])  )

(define (resolve-condition effects condition)
  (match condition
    [(list conditional operand)
     (list conditional
           (resolve-operand effects operand))]
    [(list conditional operand₀ operand₁)
     (list conditional
           (resolve-operand effects operand₀)
           (resolve-operand effects operand₁))]))

(define (path-condition summaries loops path)
  (for/list ([pc (in-list (cons #f path))]
             [next-pc (in-list path)])
    (if (exact-nonnegative-integer? pc)
      (match-let ([(list simple-cost locals-summary stack-summary effects final) (hash-ref summaries pc)])
        (match final
          [`(branch ,condition ,jump-pc ,succ-pc)
           (let ([condition (resolve-condition effects condition)])
             (cond
               [(= next-pc jump-pc)
                `(pos ,condition)]
               [(= next-pc succ-pc)
                `(neg ,condition)]
               [else
                (error 'path-condition "not jump or succ")]))]
        [`(goto ,_) #f]))
      pc)))

(require racket/list)

(define (effect-calls effects)
  (filter-map
   (λ (e)
     (match e
       [(cons (app symbol->string (regexp #rx"^invoke")) _) e]
       [_ #f]))
   effects))

(define summary-calls
  (match-lambda
    [(list simple-cost locals-summary stack-summary effects final)
     (effect-calls effects)]))

(provide summarize
         path-condition
         summary-calls)
|#

(define (dotfile m -> summaries)
  (let ([path (string-append (regexp-replace* #px"\\W" (jvm-method-name m) "") ".v")])
    (unless (file-exists? path)
      (call-with-output-file path
        (λ (op)
          (displayln "digraph G {" op)
          (for* ([(pc succ-pcs) (in-hash ->)]
                 [succ-pc (in-list succ-pcs)])
            (fprintf op "  ~a -> ~a;\n" pc succ-pc))
          (displayln "}" op))))))

(provide dotfile)
