#lang racket/base
(require web-server/servlet-env
         "main.rkt")

(serve/servlet start
               #:command-line? #t
               #:port 8080
               #:servlet-regexp #rx""
               #:stateless? #f
               #:manager manager)
