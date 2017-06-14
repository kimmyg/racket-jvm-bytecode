#lang racket/base
(require racket/pretty)
(current-print pretty-print)

(require racket/match)

(define (read-u1 ip) (read-byte ip))

(define (read-s1 ip)
  (let ([b (read-u1 ip)])
    (if (>= b (expt 2 7)) (- b (expt 2 8)) b)))

(define (read-u2 ip)
  (integer-bytes->integer (read-bytes 2 ip) #f #t))

(define (read-s2 ip)
  (let ([s (read-u2 ip)])
    (if (>= s (expt 2 15)) (- s (expt 2 16)) s)))

(define (read-u4 ip)
  (integer-bytes->integer (read-bytes 4 ip) #f #t))

(define (read-s4 ip)
  (let ([i (read-u4 ip)])
    (if (>= i (expt 2 31)) (- i (expt 2 32)) i)))

(define (read-class-info ip)
  `(class-info ,(read-u2 ip)))

(define (read-cp-info ip)
  (define (read-fieldref-info ip)
    `(fieldref-info ,(read-u2 ip)
                    ,(read-u2 ip)))
  (define (read-methodref-info ip)
    `(methodref-info ,(read-u2 ip)
                     ,(read-u2 ip)))
  (define (read-interface-methodref-info ip)
    `(interface-methodref-info ,(read-u2 ip)
                               ,(read-u2 ip)))
  (define (read-string-info ip)
    `(string-info ,(read-u2 ip)))
  (define (read-integer-info ip)
    `(integer-info ,(read-u4 ip)))
  (define (read-float-info ip)
    `(float-info ,(floating-point-bytes->real (read-bytes 4 ip) #t)))
  (define (read-long-info ip)
    `(long-info ,(integer-bytes->integer (read-bytes 8 ip) #f #t)))
  (define (read-double-info ip)
    `(double-info ,(floating-point-bytes->real (read-bytes 8 ip) #t)))
  (define (read-name-and-type-info ip)
    `(name-and-type-info ,(read-u2 ip)
                         ,(read-u2 ip)))
  (define (read-utf-8-info ip)
    (let ([bytes (read-bytes (read-u2 ip) ip)])
      `(utf-8-info ,(with-handlers ([exn:fail? (λ (e) bytes)])
                      (bytes->string/utf-8 bytes)))))
  (define (read-method-handle-info ip)
    `(method-handle-info ,(read-byte ip) ,(read-u2 ip)))
  (define (read-method-type-info ip)
    `(method-type-info ,(read-u2 ip)))
  (define (read-invoke-dynamic-info ip)
    `(invoke-dynamic-info ,(read-u2 ip)
                          ,(read-u2 ip)))
  (match (read-byte ip)
    #;[b (printf "type ~a\n" b) (failure-cont)]
    [7  (values (read-class-info ip) #f)]
    [9  (values (read-fieldref-info ip) #f)]
    [10 (values (read-methodref-info ip) #f)]
    [11 (values (read-interface-methodref-info ip) #f)]
    [8  (values (read-string-info ip) #f)]
    [3  (values (read-integer-info ip) #f)]
    [4  (values (read-float-info ip) #f)]
    [5  (values (read-long-info ip) #t)]
    [6  (values (read-double-info ip) #t)]
    [12 (values (read-name-and-type-info ip) #f)]
    [1  (values (read-utf-8-info ip) #f)]
    [15 (values (read-method-handle-info ip) #f)]
    [16 (values (read-method-type-info ip) #f)]
    [18 (values (read-invoke-dynamic-info ip) #f)]))

(define (read-interface ip)
  (read-class-info ip))

(define (read-attribute ip)
  `(attribute-info ,(read-u2 ip)
                   ,(read-bytes (read-u4 ip) ip)))

(define (read-field ip)
  `(field-info ,(read-u2 ip)
               ,(read-u2 ip)
               ,(read-u2 ip)
               ,(for/list ([i (in-range (read-u2 ip))])
                  (read-attribute ip))))

(define (read-method ip)
  `(method-info ,(read-u2 ip)
                ,(read-u2 ip)
                ,(read-u2 ip)
                ,(for/list ([i (in-range (read-u2 ip))])
                   (read-attribute ip))))

(define (read-class ip)
  (define magic (read-bytes 4 ip))
  (define minor-version (read-u2 ip))
  (define major-version (read-u2 ip))
  (define cp-infos
    (let loop ([remaining (sub1 (read-u2 ip))])
      (if (zero? remaining)
        null
        (let-values ([(x double?) (read-cp-info ip)])
          (if double?
            (list* x #f (loop (- remaining 2)))
            (cons x (loop (sub1 remaining))))))))
  (define access-flags (read-u2 ip))
  (define this-class (read-u2 ip))
  (define super-class (read-u2 ip))
  (define interfaces
    (for/list ([i (in-range (read-u2 ip))])
      (read-interface ip)))
  (define fields
    (for/list ([i (in-range (read-u2 ip))])
      (read-field ip)))
  (define methods
    (for/list ([i (in-range (read-u2 ip))])
      (read-method ip)))
  (define attributes
    (for/list ([i (in-range (read-u2 ip))])
      (read-attribute ip)))
  (list magic
        minor-version
        major-version
        cp-infos
        access-flags
        this-class
        super-class
        interfaces
        fields
        methods
        attributes))

(define (read-padding ip n)
  (let-values ([(l c p) (port-next-location ip)])
    (let ([r (remainder (sub1 p) n)])
      (unless (zero? r)
        (read-bytes (- n r) ip)))))

(define (read-instruction ip)
  (match (read-u1 ip)
    [(? eof-object?) #f]
    [1   'aconst-null]
    [2   '(iconst -1)]
    [3   '(iconst 0)]
    [4   '(iconst 1)]
    [5   '(iconst 2)]
    [6   '(iconst 3)]
    [7   '(iconst 4)]
    [8   '(iconst 5)]
    [9   '(lconst 0)]
    [10  '(lconst 1)]
    [11  '(fconst 0)]
    [12  '(fconst 1)]
    [13  '(fconst 2)]
    [14  '(dconst 0)]
    [15  '(dconst 1)]
    [16  `(bipush ,(read-s1 ip))]
    [17  `(sipush ,(read-s2 ip))]
    [18  `(ldc ,(read-u1 ip))]
    [19  `(ldc-w ,(read-u2 ip))]
    [20  `(ldc2-w ,(read-u2 ip))]
    [21  `(iload ,(read-u1 ip))]
    [22  `(lload ,(read-u1 ip))]
    [23  `(fload ,(read-u1 ip))]
    [24  `(dload ,(read-u1 ip))]
    [25  `(aload ,(read-u1 ip))]
    [26  '(iload 0)]
    [27  '(iload 1)]
    [28  '(iload 2)]
    [29  '(iload 3)]
    [30  '(lload 0)]
    [31  '(lload 1)]
    [32  '(lload 2)]
    [33  '(lload 3)]
    [34  '(fload 0)]
    [35  '(fload 1)]
    [36  '(fload 2)]
    [37  '(fload 3)]
    [38  '(dload 0)]
    [39  '(dload 1)]
    [40  '(dload 2)]
    [41  '(dload 3)]
    [42  '(aload 0)]
    [43  '(aload 1)]
    [44  '(aload 2)]
    [45  '(aload 3)]
    [46  'iaload]
    [47  'laload]
    [48  'faload]
    [49  'daload]
    [50  'aaload]
    [51  'baload]
    [52  'caload]
    [53  'saload]
    [54  `(istore ,(read-u1 ip))]
    [55  `(lstore ,(read-u1 ip))]
    [56  `(fstore ,(read-u1 ip))]
    [57  `(dstore ,(read-u1 ip))]
    [58  `(astore ,(read-u1 ip))]
    [59  '(istore 0)]
    [60  '(istore 1)]
    [61  '(istore 2)]
    [62  '(istore 3)]
    [63  '(lstore 0)]
    [64  '(lstore 1)]
    [65  '(lstore 2)]
    [66  '(lstore 3)]
    [67  '(fstore 0)]
    [68  '(fstore 1)]
    [69  '(fstore 2)]
    [70  '(fstore 3)]
    [71  '(dstore 0)]
    [72  '(dstore 1)]
    [73  '(dstore 2)]
    [74  '(dstore 3)]
    [75  '(astore 0)]
    [76  '(astore 1)]
    [77  '(astore 2)]
    [78  '(astore 3)]
    [79  'iastore]
    [80  'lastore]
    [81  'fastore]
    [82  'dastore]
    [83  'aastore]
    [84  'bastore]
    [85  'castore]
    [86  'sastore]
    [87  'pop]
    [88  'pop2]
    [89  'dup]
    [90  'dup-x1]
    [91  'dup-x2]
    [92  'dup2]
    [93  'dup2-x1]
    [96  'iadd]
    [97  'ladd]
    [98  'fadd]
    [99  'dadd]
    [100 'isub]
    [101 'lsub]
    [102 'fsub]
    [103 'dsub]
    [104 'imul]
    [105 'lmul]
    [106 'fmul]
    [107 'dmul]
    [108 'idiv]
    [109 'ldiv]
    [110 'fdiv]
    [111 'ddiv]
    [112 'irem]
    [113 'lrem]
    [116 'ineg]
    [117 'lneg]
    [120 'ishl]
    [121 'lshl]
    [122 'ishr]
    [123 'lshr]
    [124 'iushr]
    [125 'lushr]
    [126 'iand]
    [127 'land]
    [128 'ior]
    [129 'lor]
    [130 'ixor]
    [132 `(iinc ,(read-u1 ip) ,(read-s1 ip))]
    [131 'lxor]
    [133 'i2l]
    [134 'i2f]
    [135 'i2d]
    [136 'l2i]
    [137 'l2f]
    [138 'l2d]
    [139 'f2i]
    [140 'f2l]
    [141 'f2d]
    [142 'd2i]
    [143 'd2l]
    [144 'd2f]
    [145 'i2b]
    [146 'i2c]
    [147 'i2s]
    [148 'lcmp]
    [149 'fcmpl]
    [150 'fcmpg]
    [151 'dcmpl]
    [152 'dcmpg]
    [153 `(ifeq ,(read-s2 ip))]
    [154 `(ifne ,(read-s2 ip))]
    [155 `(iflt ,(read-s2 ip))]
    [156 `(ifge ,(read-s2 ip))]
    [157 `(ifgt ,(read-s2 ip))]
    [158 `(ifle ,(read-s2 ip))]
    [159 `(if-icmpeq ,(read-s2 ip))]
    [160 `(if-icmpne ,(read-s2 ip))]
    [161 `(if-icmplt ,(read-s2 ip))]
    [162 `(if-icmpge ,(read-s2 ip))]
    [163 `(if-icmpgt ,(read-s2 ip))]
    [164 `(if-icmple ,(read-s2 ip))]
    [165 `(if-acmpeq ,(read-s2 ip))]
    [166 `(if-acmpne ,(read-s2 ip))]
    [167 `(goto ,(read-s2 ip))]
    [168 `(jsr ,(read-s2 ip))]
    [169 `(ret ,(read-u1 ip))]
    [170 (read-padding ip 4)
         (let ([default (read-s4 ip)]
               [lo (read-s4 ip)]
               [hi (read-s4 ip)])
           `(tableswitch ,default ,lo ,hi
                         ,(for/list ([i (in-range (add1 (- hi lo)))])
                            (read-s4 ip))))]
    [171 (read-padding ip 4)
         `(lookupswitch ,(read-s4 ip)
                        ,(for/list ([i (in-range (read-u4 ip))])
                           (list (read-s4 ip) (read-s4 ip))))]
    [172 'ireturn]
    [173 'lreturn]
    [174 'freturn]
    [175 'dreturn]
    [176 'areturn]
    [177 'return]
    [178 `(getstatic ,(read-u2 ip))]
    [179 `(putstatic ,(read-u2 ip))]
    [180 `(getfield ,(read-u2 ip))]
    [181 `(putfield ,(read-u2 ip))]
    [182 `(invokevirtual ,(read-u2 ip))]
    [183 `(invokespecial ,(read-u2 ip))]
    [184 `(invokestatic ,(read-u2 ip))]
    [185 (begin0
           `(invokeinterface ,(read-u2 ip) ,(read-u1 ip))
           (read-u1 ip))]
    [187 `(new ,(read-u2 ip))]
    [188 `(newarray ,(read-u1 ip))]
    [189 `(anewarray ,(read-u2 ip))]
    [190 'arraylength]
    [191 'athrow]
    [192 `(checkcast ,(read-u2 ip))]
    [193 `(instanceof ,(read-u2 ip))]
    [194 'monitorenter]
    [195 'monitorexit]
    [196 (match (read-byte ip)
           [132 `(wide-iinc ,(read-u2 ip) ,(read-s2 ip))]
           [opc `(wide ,(match opc)
                       ,(read-u2 ip))])]
    [197 `(multianewarray ,(read-u2 ip) ,(read-u1 ip))]
    [198 `(ifnull ,(read-s2 ip))]
    [199 `(ifnonnull ,(read-s2 ip))]
    ))

(define (read-bytecode ip)
  (let ([instr (read-instruction ip)])
    (if instr (cons instr (read-bytecode ip)) null)))

(struct exception (start-pc end-pc handler-pc catch-type) #:transparent)

(define (read-code ip)
  (define (read-exception ip)
    (exception (read-u2 ip)
               (read-u2 ip)
               (read-u2 ip)
               (read-u2 ip)))
  (define max-stack (read-u2 ip))
  (define max-locals (read-u2 ip))
  (define bytecode (read-bytecode (open-input-bytes (read-bytes (read-u4 ip) ip))))
  (define exception-table
    (for/list ([i (in-range (read-u2 ip))])
      (read-exception ip)))
  (define attributes
    (for/list ([i (in-range (read-u2 ip))])
      (read-attribute ip)))
  `(code ,max-stack
         ,max-locals
         ,bytecode
         ,exception-table
         ,attributes))

(struct class (name) #:transparent)
(struct fieldref (class name-and-type) #:transparent)
(struct interface-methodref (class name-and-type) #:transparent)
(struct methodref (class name-and-type) #:transparent)
(struct name-and-type (name type) #:transparent)

(define (resolve-constant c constant-pool)
  (match c
    [`(class-info ,name-index)
     (class (look-up-constant name-index constant-pool))]
    [`(double-info ,x) x]
    [`(fieldref-info ,class-index ,name-and-type-index)
     (fieldref (look-up-constant class-index constant-pool)
               (look-up-constant name-and-type-index constant-pool))]
    [`(float-info ,x) x]
    [`(integer-info ,x) x]
    [`(interface-methodref-info ,class-index ,name-and-type-index)
     (interface-methodref (look-up-constant class-index constant-pool)
                          (look-up-constant name-and-type-index constant-pool))]
    [`(long-info ,x) x]
    [`(name-and-type-info ,name-index ,descriptor-index)
     (name-and-type (look-up-constant name-index constant-pool)
                    (look-up-constant descriptor-index constant-pool))]
    [`(methodref-info ,class-index ,name-and-type-index)
     (methodref (look-up-constant class-index constant-pool)
                (look-up-constant name-and-type-index constant-pool))]
    [`(string-info ,string-index)
     (look-up-constant string-index constant-pool)]
    [`(utf-8-info ,x) x]))

(define (look-up-constant i constant-pool)
  (if (zero? i) #f (resolve-constant (list-ref constant-pool (sub1 i)) constant-pool)))

(define (read-element-value-pair ip)
  (match (read-byte ip)))

(define (read-annotation ip)
  (list (read-u2 ip)
        (for ([i (in-range (read-u2 ip))])
          (read-element-value-pair ip))))

(define (resolve-bytecode bytecode constant-pool)
  (define-match-expander family
    (syntax-rules ()
      [(_ pattern opcode arguments ...)
       (list (and opcode (app symbol->string (regexp pattern (not #f)))) arguments ...)]))
  (define resolve-instruction
    (match-lambda
      [(family #rx"^(a|d|f|i|l)load$" opcode n)
       (list opcode n)]
      [(family #rx"^(d|f|i|l)const$" opcode n)
       (list opcode n)]
      [(family #rx"^iinc$" opcode index constant)
       (list opcode index constant)]
      [(family #rx"^(a|d|f|i|l)store$" opcode n)
       (list opcode n)]
      [(family #rx"^if(eq|ge|gt|le|lt|ne|nonnull|null)" opcode offset)
       (list opcode offset)]
      [(family #rx"^if-acmp(eq|ne)$" opcode offset)
       (list opcode offset)]
      [(family #rx"^if-icmp(eq|ge|gt|le|lt|ne)$" opcode offset)
       (list opcode offset)]
      [`(anewarray ,index)
       `(anewarray ,(look-up-constant index constant-pool))]
      [`(bipush ,n)
       `(bipush ,n)]
      [`(checkcast ,index)
       `(checkcast ,(look-up-constant index constant-pool))]
      [`(goto ,offset)
       `(goto ,offset)]
      [`(ldc ,index)
       `(ldc ,(look-up-constant index constant-pool))]
      [`(ldc-w ,index)
       `(ldc-w ,(look-up-constant index constant-pool))]
      [`(ldc2-w ,index)
       `(ldc2-w ,(look-up-constant index constant-pool))]
      [`(lookupswitch ,de ,offsets)
       `(lookupswitch ,de ,offsets)]
      [`(jsr ,offset)
       `(jsr ,offset)]
      [`(multianewarray ,index ,count)
       `(multianewarray ,(look-up-constant index constant-pool) ,count)]
      [`(new ,index)
       `(new ,(look-up-constant index constant-pool))]
      [`(newarray ,type)
       `(newarray ,type)]
      [`(ret ,offset)
       `(ret ,offset)]
      [`(sipush ,n)
       `(sipush ,n)]
      [`(tableswitch ,de ,lo ,hi ,offsets)
       `(tableswitch ,de ,lo ,hi ,offsets)]
      [`(wide-iinc ,index ,const)
       `(wide-iinc ,index ,const)]
      [`(instanceof ,index)
       `(instanceof ,(look-up-constant index constant-pool))]
      [`(invokeinterface ,index ,count)
       `(invokeinterface ,(look-up-constant index constant-pool) ,count)]
      [`(invokespecial ,index)
       `(invokespecial ,(look-up-constant index constant-pool))]
      [`(invokestatic ,index)
       `(invokestatic ,(look-up-constant index constant-pool))]
      [`(invokevirtual ,index)
       `(invokevirtual ,(look-up-constant index constant-pool))]
      [`(getfield ,index)
       `(getfield ,(look-up-constant index constant-pool))]
      [`(putfield ,index)
       `(putfield ,(look-up-constant index constant-pool))]
      [`(getstatic ,index)
       `(getstatic ,(look-up-constant index constant-pool))]
      [`(putstatic ,index)
       `(putstatic ,(look-up-constant index constant-pool))]
      [(and opcode (or 'aaload 'aastore 'aconst-null 'areturn 'arraylength 'athrow
                       'baload 'bastore
                       'caload 'castore
                       'd2f 'd2i 'd2l 'dadd 'daload 'dastore 'dcmpg 'dcmpl 'ddiv 'dmul 'dreturn 'dsub 'dup 'dup2 'dup2-x1 'dup-x1 'dup-x2
                       'f2d 'f2i 'f2l 'fadd 'faload 'fastore 'fcmpg 'fcmpl 'fdiv 'fmul 'freturn 'fsub
                       'i2b 'i2c 'i2d 'i2f 'i2l 'i2s 'iadd 'iaload 'iand 'iastore 'idiv 'imul 'ineg 'ior 'irem 'ireturn 'ishl 'ishr 'isub 'iushr 'ixor
                       'l2d 'l2f 'l2i 'ladd 'land 'laload 'lastore 'lcmp 'ldiv 'lmul 'lneg 'lor 'lrem 'lreturn 'lshl 'lshr 'lsub 'lushr 'lxor
                       'monitorenter 'monitorexit
                       'pop 'pop2
                       'return
                       'saload 'sastore))
       opcode]))
  #;
  (define (resolve-instruction instruction stack)
    (match instruction
      [`(aload ,n)
       (cons `(aload ,n) stack)]
      [`(invokespecial ,index)
       (match (look-up-constant index constant-pool)
         [`(methodref (class ,class-name) (name-and-type ,name ,type))
          (match type
            ["()V" (match-let ([(cons target stack) stack])
                     (cons `(result (call ,target `(methodref (class ,class-name) (name-and-type ,name ,type)))) stack))])])]
      [`(putfield ,index)
       (match-let ([(list* objectref value stack) stack])
         (cons `(putfield ,(look-up-constant index constant-pool) objectref value) stack))]))
  #;
  (raise (foldl resolve-instruction null bytecode))
  #;
  (let loop ([instrs bytecode]
             [stack null])
    (match instrs
      [(list) (list)]
      [(cons instr instrs)
       (let-values ([(instr stack) (resolve-instruction instr stack)])
         (cons instr (loop instrs stack)))]))
  (map resolve-instruction bytecode))

(struct annotation (type element-value-pairs) #:transparent)

(define (resolve-annotation annotation constant-pool)
  (match-let ([(list type-index element-value-pairs) annotation])
    (annotation (look-up-constant type-index constant-pool)
                element-value-pairs)))

(struct line-number (start-pc line-number) #:transparent)
(struct local-variable (start-pc length name descriptor index) #:transparent)
(struct local-variable-type (start-pc length name descriptor index) #:transparent)

(struct top-variable-info () #:transparent)
(struct integer-variable-info () #:transparent)
(struct float-variable-info () #:transparent)
(struct double-variable-info () #:transparent)
(struct long-variable-info () #:transparent)
(struct null-variable-info () #:transparent)
(struct uninitialized-this-variable-info () #:transparent)
(struct object-variable-info (class) #:transparent)
(struct uninitialized-variable-info (offset) #:transparent)

(define (resolve-attribute attribute constant-pool)
  (match-let ([`(attribute-info ,name-index ,payload) attribute])
    (match (look-up-constant name-index constant-pool)
      ["AnnotationDefault"
       (cons 'AnnotationDefault
             payload)]
      ["Code"
       (cons 'Code
             (resolve-code (read-code (open-input-bytes payload)) constant-pool)) ]
      ["ConstantValue"
       (cons 'ConstantValue
             (look-up-constant (read-u2 (open-input-bytes payload)) constant-pool) )]
      ["Deprecated"
       (cons 'Deprecated
             #t)]
      ["Exceptions"
       (cons 'Exceptions
             (let ([ip (open-input-bytes payload)])
               (for/list ([i (in-range (read-u2 ip))])
                 (look-up-constant (read-u2 ip) constant-pool))))]
      ["LineNumberTable"
       (cons 'LineNumberTable
             (let ([ip (open-input-bytes payload)])
               (for/list ([i (in-range (read-u2 ip))])
                 (line-number (read-u2 ip)
                              (read-u2 ip))))) ]
      ["LocalVariableTable"
       (cons 'LocalVariableTable
             (let ([ip (open-input-bytes payload)])
               (for/list ([i (in-range (read-u2 ip))])
                 (local-variable (read-u2 ip)
                                 (read-u2 ip)
                                 (look-up-constant (read-u2 ip) constant-pool)
                                 (look-up-constant (read-u2 ip) constant-pool)
                                 (read-u2 ip)))))]
      ["LocalVariableTypeTable"
       (cons 'LocalVariableTypeTable
             (let ([ip (open-input-bytes payload)])
               (for/list ([i (in-range (read-u2 ip))])
                 (local-variable-type (read-u2 ip)
                                      (read-u2 ip)
                                      (look-up-constant (read-u2 ip) constant-pool)
                                      (look-up-constant (read-u2 ip) constant-pool)
                                      (read-u2 ip)))))]
      ["RuntimeInvisibleAnnotations"
       (cons 'RuntimeInvisibleAnnotations
             payload
             #;
             (let ([ip (open-input-bytes payload)])
               (for/list ([i (in-range (read-u2 ip))])
                 (resolve-annotation (read-annotation ip) constant-pool))))]
      ["RuntimeInvisibleParameterAnnotations"
       (cons 'RuntimeInvisibleParameterAnnotations
             payload)]
      ["RuntimeVisibleAnnotations"
       (cons 'RuntimeVisibleAnnotations
             payload
             #;
             (let ([ip (open-input-bytes payload)])
               (for/list ([i (in-range (read-u2 ip))])
                 (resolve-annotation (read-annotation ip) constant-pool))))]
      ["RuntimeVisibleParameterAnnotations"
       (cons 'RuntimeVisibleParameterAnnotations
             payload)]
      ["Signature"
       (cons 'Signature
             (look-up-constant (read-u2 (open-input-bytes payload)) constant-pool))]
      ["StackMapTable"
       (define (read-verification-type-info ip)
         (let ([tag (read-byte ip)])
           (cond
             [(= tag 0) (top-variable-info)]
             [(= tag 1) (integer-variable-info)]
             [(= tag 2) (float-variable-info)]
             [(= tag 3) (double-variable-info)]
             [(= tag 4) (long-variable-info)]
             [(= tag 5) (null-variable-info)]
             [(= tag 6) (uninitialized-this-variable-info)]
             [(= tag 7) (object-variable-info (look-up-constant (read-u2 ip) constant-pool))]
             [(= tag 8) (uninitialized-variable-info (read-u2 ip))]
             [else
              (error 'read-verification-type-info "bad tag ~a" tag)])))
       (define (read-stack-map-frame ip)
         (let ([frame-type (read-byte ip)])
           (cond
             [(< frame-type 64) `(same-frame ,frame-type)]
             [(< frame-type 128) `(same-locals-1-stack-item-frame ,frame-type (,(read-verification-type-info ip)))]
             [(< frame-type 247) (error 'read-stack-map-frame "bad frame type ~a" frame-type)]
             [(< frame-type 248) `(same-locals-1-stack-item-frame-extended ,frame-type ,(read-u2 ip) (,(read-verification-type-info ip)))]
             [(< frame-type 251) `(chop-frame ,frame-type ,(read-u2 ip))]
             [(< frame-type 252) `(same-frame-extended ,frame-type ,(read-u2 ip))]
             [(< frame-type 255) `(append-frame ,frame-type
                                                ,(read-u2 ip)
                                                ,(for/list ([i (in-range (- frame-type 251))])
                                                   (read-verification-type-info ip)))]
             [(< frame-type 256) `(full-frame ,frame-type
                                              ,(read-u2 ip)
                                              ,(for/list ([i (in-range (read-u2 ip))])
                                                 (read-verification-type-info ip))
                                              ,(for/list ([i (in-range (read-u2 ip))])
                                                 (read-verification-type-info ip)))]
             [else
              (error 'read-stack-map-frame "bad frame type ~a" frame-type)])))
       (cons 'StackMapTable
             (let ([ip (open-input-bytes payload)])
               (for/list ([i (in-range (read-u2 ip))])
                 (read-stack-map-frame ip))))]
      ["Synthetic"
       (cons 'Synthetic #t)])))

(struct code (max-stack max-locals bytecode exception-table attributes) #:transparent)

(define (resolve-code code* constant-pool)
  (match-let ([`(code ,max-stack ,max-locals ,bytecode ,exception-table ,attributes) code*])
    (code max-stack
          max-locals
          (resolve-bytecode bytecode constant-pool)
          exception-table
          (for/list ([attribute (in-list attributes)])
            (resolve-attribute attribute constant-pool)))))

(struct jvm-class (this super access-flags interfaces fields methods) #:transparent)
(struct jvm-field (access-flags name descriptor attributes) #:transparent)
(struct jvm-method (access-flags name descriptor attributes) #:transparent)

(define resolve-class
  (match-lambda
    [(list #"\312\376\272\276" minor-version major-version
           constant-pool
           access-flags this-class super-class
           interfaces fields methods attributes)
     (jvm-class (look-up-constant this-class constant-pool)
                (look-up-constant super-class constant-pool)
                (for/list ([v (in-list '(#x1 #x10 #x20 #x200 #x400 #x1000 #x2000 #x4000))]
                           [p (in-list '(PUBLIC FINAL SUPER INTERFACE ABSTRACT SYNTHETIC ANNOTATION ENUM))]
                           #:when (not (zero? (bitwise-and access-flags v))))
                  p)
                (for/list ([interface (in-list interfaces)])
                  (match-let ([`(class-info ,name-index) interface])
                    (look-up-constant name-index constant-pool)))
                (for/list ([field (in-list fields)])
                  (match-let ([`(field-info ,access-flags ,name-index ,descriptor-index ,attributes) field])
                    (jvm-field (for/list ([v (in-list '(#x1 #x2 #x4 #x8 #x10 #x20 #x40 #x80 #x1000 #x4000))]
                                          [p (in-list '(PUBLIC PRIVATE PROTECTED STATIC FINAL VOLATILE TRANSIENT SYNTHETIC ENUM))]
                                          #:when (not (zero? (bitwise-and access-flags v))))
                                 p)
                               (look-up-constant name-index constant-pool)
                               (look-up-constant descriptor-index constant-pool)
                               (for/list ([attribute (in-list attributes)])
                                 (resolve-attribute attribute constant-pool)))))
                (for/list ([method (in-list methods)])
                  (match-let ([`(method-info ,access-flags ,name-index ,descriptor-index ,attributes) method])
                    (jvm-method (for/list ([v (in-list '(#x1 #x2 #x4 #x8 #x10 #x20 #x40 #x80 #x100 #x400 #x800 #x1000))]
                                           [p (in-list '(PUBLIC PRIVATE PROTECTED STATIC FINAL SYNCHRONIZED BRIDGE VARARGS NATIVE ABSTRACT STRICT SYNTHETIC))]
                                           #:when (not (zero? (bitwise-and access-flags v))))
                                  p)
                                (look-up-constant name-index constant-pool)
                                (look-up-constant descriptor-index constant-pool)
                                (for/list ([attribute (in-list attributes)])
                                  (resolve-attribute attribute constant-pool))))))]))

(require racket/file
         racket/system)

(define (extract-classes key . classpaths)
  (let ([base-path (build-path (find-system-path 'temp-dir) key)])
    #;
    (for* ([classpath (in-list classpaths)]
           [path (in-list (directory-list classpath #:build? #t))])
      (match path
        [(app path->string (regexp #rx"([^/]*)\\.jar$" (list _ base)))
         (let ([temp-subpath (build-path base-path base)])
           (make-directory* temp-subpath)
           (parameterize ([current-directory temp-subpath])
             (system (format "unzip -o ~s" (path->string path)))))]))
    base-path))

(define jar-dir "/Users/kimballg/Development/STAC/challenge_programs/airplan_1/challenge_program/lib")

(define class-path (extract-classes "airplan_1" jar-dir))

(for ([path (in-directory class-path)]
      #:when (regexp-match? #rx"\\.class$" path))
  (pretty-print (resolve-class (call-with-input-file path read-class))))


