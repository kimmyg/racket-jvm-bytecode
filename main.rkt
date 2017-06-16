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

(require "loop.rkt")

(require "descriptor.rkt")

#;
(for* ([c (in-list classes)]
            [m (in-list (jvm-class-methods c))]
            #:when (assq 'Code (jvm-method-attributes m)))
  ((current-print) (loops m)))

(match (classes-with-main classes)
  [(list c)
   ((current-print) (loops (method/name c "main")))])

#;
(match (classes-with-main classes)
  [(list c)
   (parse-method-descriptor (jvm-method-descriptor (method/name c "main")))])


#|
(require "hierarchy.rkt")
#;
(for-each register-class! classes)

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



#;
(loops m)


|#
