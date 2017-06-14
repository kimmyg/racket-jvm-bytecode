#lang racket/base
(require db
         racket/match
         "data.rkt")

(define conn (sqlite3-connect #:database 'memory))

(query-exec conn "CREATE TABLE subclasses (
                    cn TEXT NOT NULL, -- class name
                    scn TEXT NOT NULL, -- superclass name
                    PRIMARY KEY ( cn, scn )
                  );")

(query-exec conn "CREATE TABLE implements (
                    cn TEXT NOT NULL, -- class name
                    ifn TEXT NOT NULL, -- interface name
                    PRIMARY KEY ( cn, ifn )
                  );")

(define (register-class! c)
  (match* ((jvm-class-this c) (jvm-class-super c))
    [((class this-class) (class super-class))
     (query-exec conn "INSERT INTO subclasses ( cn, scn ) VALUES ( ?1, ?2 );" this-class super-class)
     (for ([i (in-list (jvm-class-interfaces c))])
       (match i
         [(class interface)
          (query-exec conn "INSERT INTO implements ( cn, ifn ) VALUES ( ?1, ?2 );" this-class interface)]))]))

(provide register-class!)

#;(query-rows conn "SELECT ifn, count(*) FROM implements GROUP BY ifn;")
#;(query-rows conn "SELECT scn, count(*) FROM subclasses GROUP BY scn;")



