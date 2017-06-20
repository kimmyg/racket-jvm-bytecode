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

(define (methods/name c name)
  (filter (λ (m) (string=? (jvm-method-name m) name)) (jvm-class-methods c)))

(require "hierarchy.rkt")
(register-classes! classes)

(require "summary.rkt"
         "loops.rkt")

(define select
  (match-lambda
    [(list x) x]
    [xs (error 'select "more than one: ~v" xs)]))

(require racket/set)

(define refit
  (match-lambda
    [(list* c m rst)
     (list* (jvm-class-this c)
            (name-and-type (jvm-method-name m)
                           (jvm-method-descriptor m))
            rst)]))

(define targets
  (match-lambda
    [(or `(invokespecial ,_ ,methodref ,_ ,_)
         `(invokestatic ,methodref ,_ ,_)
         `(invokeinterface ,_ ,methodref ,_ ,_)
         `(invokevirtual ,_ ,methodref ,_ ,_))
     (resolve-methodref methodref)]))

; class method -> set of edges (class method parent-loop,class method)
(define (calls c m)
  (define-values (summaries -> <-) (summarize m))
  (dotfile m -> summaries)
  (define method-loops (loops -> <-))
  ((current-print) (refit (list c m)))
  ((current-print) method-loops)
  
  #;
  (for ([(loop blocks) (in-hash method-loops)])
    ((current-print) (path-condition summaries blocks)))

  #;
  ((current-print) loop-hierarchy)
  
  #;
  (define (loop-header x)
    (if (exact-nonnegative-integer? x)
        x
        (match x
          [(loop x _) (loop-header x)])))
  #;
  (define loop-membership (for/fold ([mem (hasheqv)]) ([loop (in-list method-loops)])
                            (let ([loop-name (match-let ([(list header _ ... footer) loop])
                                               (loop (loop-header header) footer))])
                              (for/fold ([mem mem]) ([pc (in-list loop)])
                                (if (exact-nonnegative-integer? pc)
                                    (hash-set mem loop-name)
                                    mem)))))
  #;
  (displayln 'METHOD)
  #;
  ((current-print) (refit (list c m)))
  #;
  (let loop ([todo (list 0)]
             [seen (seteqv)]
             [edges (set)])
    (match todo
      [(cons pc todo)
       (if (set-member? seen pc)
           (loop todo seen edges)
           (loop (append (hash-ref -> pc null) todo)
                 (set-add seen pc)
                 (for*/fold ([edges edges]) ([call (in-list (summary-calls (hash-ref summaries pc)))]
                                             [target (in-list (targets call))])
                   (displayln 'CALL)
                   ((current-print) call)
                   (displayln 'TARGET)
                   ((current-print) (refit target))
                   (set-add edges (list (list c m (hash-ref loop-membership pc #f)) target)))))]
      [(list)
       edges])))

(for* ([c (in-list classes)]
       [m (in-list (jvm-class-methods c))]
       #:when (assq 'Code (jvm-method-attributes m)))
  (calls c m))

#;
(let* ([c (select (classes-with-main classes))]
       [m (select (methods/name c "main"))])
  (calls c m)
  
  #;
  (let loop ([todo (list (list c m))]
             [seen (set)]
             [edges (set)])
    (match todo
      [(cons (and cm (list c m)) todo)
       (if (set-member? seen cm)
         (loop todo seen edges)
         (let ([seen (set-add seen cm)])
           (let-values ([(todo edges) (for/fold ([todo todo] [edges edges])
                                                ([edge (in-set (calls c m))])
                                        (values (match-let ([(list src dst) edge])
                                                  (cons dst todo))
                                                (match-let ([(list src dst) edge])
                                                  (set-add edges (list (refit src) (refit dst))))))])
             (loop todo seen edges))))]
      [(list)
       edges])))

; WEB

(require racket/runtime-path)
(define-runtime-path apps-path "/Users/kimballg/Development/STAC/challenge_programs")

(require web-server/servlet/web
         web-server/managers/lru
         web-server/dispatch
         web-server/http/redirect
         web-server/http/xexpr)

(define (page/app req app-name)
  42)

(define (page/main req)
  (response/xexpr
   `(html
     (body
      (ul
       ,@(for/list ([app (in-list (directory-list apps-path))])
           (let ([app-name (path->string app)])
             `(li (a ([href ,(make-url page/app app-name)]) ,app-name)))))))))

(define (redirect/main req)
  (redirect-to (make-url page/main)))

(define-values (start make-url)
  (dispatch-rules))

(define manager (make-threshold-LRU-manager redirect/main (* 1024 1024 512)))

(define interface-version 'v2)

(provide start interface-version manager)

#;
(for* ([c (in-list classes)]
       [m (in-list (jvm-class-methods c))]
       #:when (assq 'Code (jvm-method-attributes m)))
  ((current-print) (jvm-class-this c))
  ((current-print) (jvm-method-name m))
  (define-values (summaries -> <-) (summarize m))
  (define-values (method-loops loop-hierarchy) (loops -> <-))
  ((current-print) loop-hierarchy)
  (for ([loop (in-list method-loops)])
    (displayln "A LOOP")
    ((current-print) loop)
    (for ([pc (in-list loop)]
          #:when (exact-nonnegative-integer? pc))
      ((current-print) (summary-calls (hash-ref summaries pc))))))

#;
(match (classes-with-main classes)
  [(list c)
   ((current-print) (loops (method/name c "main")))])

#;
(match (classes-with-main classes)
  [(list c)
   (parse-method-descriptor (jvm-method-descriptor (method/name c "main")))])


#|

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
