#lang racket/base
(require racket/pretty)
(current-print pretty-print)

(require racket/match
         "read.rkt"
         "data.rkt"
         "resolve.rkt"
         "instruction.rkt")

(require "extract.rkt")

(define app-path "/Users/kimballg/Development/STAC/challenge_programs/malware_analyzer")

(define classes
  (for/list ([call-with-class-input-stream (in-list (extract app-path))])
    (resolve-class (call-with-class-input-stream read-class))))

(define (classes-with-main cs)
  (filter
   (λ (c)
     (findf
      (λ (method)
        (and (string=? (jvm-method-name method) "main")
             (memq 'PUBLIC (jvm-method-access-flags method))
             (memq 'STATIC (jvm-method-access-flags method))))
      (jvm-class-methods c)))
   cs))

(define (method/name c name)
  (match (filter (λ (m) (string=? (jvm-method-name m) name)) (jvm-class-methods c))
    [(list m) m]
    [_ (error 'method/name "multiple methods with name ~s" name)]))

(require "descriptor.rkt")

(for*/list ([c (in-list classes)]
            [f (in-list (jvm-class-fields c))])
  (parse-field-descriptor (jvm-field-descriptor f)))

#;
(match (classes-with-main classes)
  [(list c)
   (parse-method-descriptor (jvm-method-descriptor (method/name c "main")))])


#|
(require "hierarchy.rkt")
#;
(for-each register-class! classes)

#;
(query-rows conn "WITH RECURSIVE trans ( cn, tscn ) AS (
                    SELECT cn, scn FROM subclasses UNION ALL SELECT trans.cn")
#;
(code-bytecode (cdr (assq 'Code (jvm-method-attributes m))))


#;
(match (classes-with-main (hash-values classes))
  [(list c) (method/name c "main")])



(define (finda f xs)
  (match xs
    [(list) #f]
    [(cons x xs)
     (let ([y (f x)]) (if y y (finda f xs)))]))

(define (>>= f v) (and v (f v)))

#;
(define m
 (finda
  (λ (c)
    (findf
     (λ (m)
       (>>= (λ (code)
              (findf
               (match-lambda
                 [(instruction _  (family #rx"^if" _ _)) #t]
                 [_ #f])
               (code-bytecode (cdr code))))
            (assq 'Code (jvm-method-attributes m))))
     (jvm-class-methods c)))
  classes))

(require racket/set)


(define (loops m)
  (define (blocks instrs)
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
                   [(family #rx"^if" _ offset)
                    (set-add (set-add boundaries next)
                             (if (< offset 0)
                               (pred prev next (+ pc offset))
                               (succ prev next (+ pc offset))))]
                   [_ boundaries])
                 (cons instr prev) next*)]
          [(list) boundaries])))
    (let ([bs (block-boundaries instrs)])
      (let loop ([block null]
                 [instrs instrs])
        (match instrs
          [(cons instr instrs*)
           (if (set-member? bs instrs)
             (cons (reverse block) (loop (list instr) instrs*))
             (loop (cons instr block) instrs*))]
          [(list)
           (list (reverse block))]))))
  (blocks (code-bytecode (cdr (assq 'Code (jvm-method-attributes m))))))

#;
(loops m)


|#
