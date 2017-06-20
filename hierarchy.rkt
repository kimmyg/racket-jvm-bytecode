#lang racket/base
(require db
         racket/match
         "data.rkt")

(define conn (sqlite3-connect #:database 'memory))

(define (fresh-database!)
  (query-exec conn "DROP TABLE IF EXISTS subclasses;")
  (query-exec conn "CREATE TABLE subclasses (
                      cn TEXT NOT NULL, -- class name
                      scn TEXT NOT NULL, -- superclass name
                      PRIMARY KEY ( cn, scn )
                    );")
  (query-exec conn "DROP TABLE IF EXISTS implements;")
  (query-exec conn "CREATE TABLE implements (
                      cn TEXT NOT NULL, -- class name
                      ifn TEXT NOT NULL, -- interface name
                      PRIMARY KEY ( cn, ifn )
                    );"))

(define classes #f)

(define (register-classes! cs)
  (set! classes
        (for/hash ([c (in-list cs)])
          (values (class-name (jvm-class-this c)) c)))
  (fresh-database!)
  (for ([c (in-list cs)])
    (match* ((jvm-class-this c) (jvm-class-super c))
      [((class this-class) (class super-class))
       (query-exec conn "INSERT INTO subclasses ( cn, scn ) VALUES ( ?1, ?2 );" this-class super-class)
       (for ([i (in-list (jvm-class-interfaces c))])
         (match i
           [(class interface)
            (query-exec conn "INSERT INTO implements ( cn, ifn ) VALUES ( ?1, ?2 );" this-class interface)]))])))

(define resolve-methodref
  (match-lambda
    [(interface-methodref (class ifn) (name-and-type method-name method-type))
     (for*/list ([cn (in-query conn "SELECT cn FROM implements WHERE ifn = ?1;" ifn)]
                 [c (in-value (hash-ref classes cn))]
                 [m (in-list (jvm-class-methods c))]
                 #:when (and (equal? (jvm-method-name m) method-name)
                             (equal? (jvm-method-descriptor m) method-type)))
       (list c m))]
    [(methodref (class cn) (name-and-type method-name method-type))
     (for*/list ([cn (in-query conn "SELECT cn FROM subclasses WHERE cn = ?1 OR scn = ?1;" cn)]
                 [c (in-value (hash-ref classes cn))]
                 [m (in-list (jvm-class-methods c))]
                 #:when (and (equal? (jvm-method-name m) method-name)
                             (equal? (jvm-method-descriptor m) method-type)))
       (list c m))]))

(provide register-classes! resolve-methodref)

#;(query-rows conn "SELECT ifn, count(*) FROM implements GROUP BY ifn;")
#;(query-rows conn "SELECT scn, count(*) FROM subclasses GROUP BY scn;")
#;
(query-rows conn "WITH RECURSIVE trans ( cn, tscn ) AS (
                    SELECT cn, scn FROM subclasses UNION ALL SELECT trans.cn")
