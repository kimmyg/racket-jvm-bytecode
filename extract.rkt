#lang racket/base
(require racket/file
         racket/match
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

(provide extract)
