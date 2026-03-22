#lang racket/base

(require racket/string
         web-server/http/request-structs)

(provide accepts-encoding?)

(define (accepts-encoding? req encoding)
  (define enc-str (symbol->string encoding))
  (define accept-header
    (for/or ([h (in-list (request-headers/raw req))])
      (and (equal? (string-downcase (bytes->string/utf-8 (header-field h))) "accept-encoding")
           (header-value h))))
  (and accept-header
       (for/or ([part (in-list (regexp-split #rx"," (bytes->string/utf-8 accept-header)))])
         (string-ci=? (string-trim (car (string-split part ";"))) enc-str))))
