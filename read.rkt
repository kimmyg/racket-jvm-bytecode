#lang racket/base
(require racket/match)

(define (read-u1 ip)
  (read-byte ip))

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
    [119 'dneg]
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
  (define (port-location ip) (let-values ([(_₀ _₁ p) (port-next-location ip)]) (sub1 p)))
  (define base-pc (port-location ip))
  (define (read-bytecode ip)
    (let ([pc (- (port-location ip) base-pc)]
          [instr (read-instruction ip)])
      (if instr (cons (list pc instr) (read-bytecode ip)) null)))
  (read-bytecode ip))

(define (read-code ip)
  (define (read-exception ip)
    (list (read-u2 ip)
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

(define (read-element-value-pair ip)
  (match (read-byte ip)))

(define (read-annotation ip)
  (list (read-u2 ip)
        (for ([i (in-range (read-u2 ip))])
          (read-element-value-pair ip))))

(provide (all-defined-out))
