#lang racket/base

(struct instruction (pc instr) #:transparent)
(struct exception (start-pc end-pc handler-pc catch-type) #:transparent)

(struct class (name) #:transparent)
(struct fieldref (class name-and-type) #:transparent)
(struct interface-methodref (class name-and-type) #:transparent)
(struct methodref (class name-and-type) #:transparent)
(struct name-and-type (name type) #:transparent)

(struct annotation (type element-value-pairs) #:transparent)

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

(struct code (max-stack max-locals bytecode exception-table attributes) #:transparent)

(struct jvm-class (this super access-flags interfaces fields methods) #:transparent)
(struct jvm-field (access-flags name descriptor attributes) #:transparent)
(struct jvm-method (access-flags name descriptor attributes) #:transparent)

(provide (all-defined-out))
