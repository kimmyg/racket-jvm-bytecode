#lang racket/base
(require racket/match
         "data.rkt"
         "instruction.rkt"
         "read.rkt")

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

(define (resolve-bytecode bytecode constant-pool)
  (define resolve-instruction*
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
                       'd2f 'd2i 'd2l 'dadd 'daload 'dastore 'dcmpg 'dcmpl 'ddiv 'dmul 'dneg 'dreturn 'dsub 'dup 'dup2 'dup2-x1 'dup-x1 'dup-x2
                       'f2d 'f2i 'f2l 'fadd 'faload 'fastore 'fcmpg 'fcmpl 'fdiv 'fmul 'freturn 'fsub
                       'i2b 'i2c 'i2d 'i2f 'i2l 'i2s 'iadd 'iaload 'iand 'iastore 'idiv 'imul 'ineg 'ior 'irem 'ireturn 'ishl 'ishr 'isub 'iushr 'ixor
                       'l2d 'l2f 'l2i 'ladd 'land 'laload 'lastore 'lcmp 'ldiv 'lmul 'lneg 'lor 'lrem 'lreturn 'lshl 'lshr 'lsub 'lushr 'lxor
                       'monitorenter 'monitorexit
                       'pop 'pop2
                       'return
                       'saload 'sastore))
       opcode]))
  (define resolve-instruction
    (match-lambda
      [(list pc instr)
       (instruction pc (resolve-instruction* instr))]))
  (map resolve-instruction bytecode))

(define (resolve-annotation annotation constant-pool)
  (match-let ([(list type-index element-value-pairs) annotation])
    (annotation (look-up-constant type-index constant-pool)
                element-value-pairs)))

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

(define (resolve-code code* constant-pool)
  (match-let ([`(code ,max-stack ,max-locals ,bytecode ,exception-table ,attributes) code*])
    (code max-stack
          max-locals
          (resolve-bytecode bytecode constant-pool)
          (map
           (match-lambda
             [(list start-pc end-pc handler-pc catch-type)
              (exception start-pc end-pc handler-pc (look-up-constant catch-type constant-pool))])
           exception-table)
          (for/list ([attribute (in-list attributes)])
            (resolve-attribute attribute constant-pool)))))

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

(provide (all-defined-out))
