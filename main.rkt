#lang racket/base
(require racket/pretty)
(current-print pretty-print)

(require racket/match
         "read.rkt"
         "data.rkt"
         "resolve.rkt"
         "instruction.rkt")







(require racket/file
         racket/system)

(define (extract base-path)
  (let ([base-temp-path (match (regexp-match #rx"([^/]*)$" base-path)
                          [(list _ key)
                           (build-path (find-system-path 'temp-dir) key)])])
    (for ([path (in-directory base-path)])
      (match (regexp-match #rx"([^/]*)\\.jar$" path)
        [(list _ base)
         (let ([base-temp-subpath (build-path base-temp-path base)])
           (make-directory* base-temp-subpath)
           (parameterize ([current-directory base-temp-subpath])
             (system (format "unzip -u -qq ~s" (path->string path)))))]
        [#f
         (void)]))
    (for/list ([path (in-directory base-temp-path)]
               #:when (regexp-match? #rx"\\.class$" path))
      (λ (f) (call-with-input-file path f)))))

#;
(define (extract-classes key . classpaths)
  (let ([base-path (build-path (find-system-path 'temp-dir) key)])
    (for* ([classpath (in-list classpaths)]
           [path (in-list (directory-list classpath #:build? #t))])
      (match path
        [(app path->string (regexp #rx"([^/]*)\\.jar$" (list _ base)))
         (let ([temp-subpath (build-path base-path base)])
           (make-directory* temp-subpath)
           (parameterize ([current-directory temp-subpath])
             (system (format "unzip -u  ~s" (path->string path)))))] ; add -qq to unzip
        [_ (void)]))
    base-path))

(extract )

#;(define jar-dir "/Users/kimballg/Development/STAC/challenge_programs/malware_analyzer/challenge_program")
#;(define class-path (extract-classes "malware-analyzer" jar-dir))

#;(define jar-dir "/Users/kimballg/Development/STAC/challenge_programs/airplan_1/challenge_program/lib")
#;(define class-path (extract-classes "airplan-1" jar-dir))

(define app-path "/Users/kimballg/Development/STAC/challenge_programs/airplan_1")

(define classes
  (for/list ([call-with-class-stream (in-list (extract app-path))])
    (resolve-class (call-with-class-stream read-class))))

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



