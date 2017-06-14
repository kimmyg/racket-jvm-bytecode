#lang racket/base
(require racket/match)

(struct field-type () #:transparent)

(struct base-type field-type (raw) #:transparent)
(struct object-type field-type (class-name) #:transparent)
(struct array-type field-type (component) #:transparent)

(struct return-type () #:transparent)

(struct return-field return-type (field) #:transparent)
(struct return-void return-type () #:transparent)

(struct method-descriptor (parameters return) #:transparent)

(define (expect! ip c₀)
  (let ([c (read-char ip)])
    (unless (eqv? c c₀) (error 'expect! "expected ~v; got ~v" c₀ c))))

(define (next? ip c₀)
  (and (eqv? (peek-char ip) c₀) (read-char ip)))

(define (parse-field-type ip)
  (match (read-char ip)
    [(and type (or #\B #\C #\D #\F #\I #\J #\S #\Z))
     (base-type (string type))]
    [#\L
     (match-let ([(list _ class-name) (regexp-match #rx"([^;]*);" ip)])
       (object-type (bytes->string/utf-8 class-name)))]
    [#\[
     (array-type (parse-field-type ip))]))

(define (parse-field-descriptor descriptor)
  (parse-field-type (open-input-string descriptor)))

(define (parse-parameter-descriptor ip)
  (parse-field-type ip))

(define (parse-return-descriptor ip)
  (if (next? ip #\V)
    (return-void)
    (return-field (parse-field-type ip))))

(define (parse-method-descriptor descriptor)
  (let ([ip (open-input-string descriptor)])
    (expect! ip #\()
    (method-descriptor
     (let loop ()
       (if (next? ip #\))
         null
         (cons (parse-parameter-descriptor ip) (loop))))
     (parse-return-descriptor ip))))

(provide (all-defined-out))
