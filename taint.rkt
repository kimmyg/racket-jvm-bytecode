#lang racket/base

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
