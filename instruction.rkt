#lang racket/base
(require racket/match)

(define-match-expander family
  (syntax-rules ()
    [(_ pattern opcode)
     (and opcode (app symbol->string (regexp pattern (not #f))))]
    [(_ pattern opcode arguments ...)
     (list (and opcode (app symbol->string (regexp pattern (not #f)))) arguments ...)]))

(provide (all-defined-out))
