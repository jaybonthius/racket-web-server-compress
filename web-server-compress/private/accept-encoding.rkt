#lang racket/base

(require racket/string
         web-server/http/request-structs)

(provide accepts-encoding?)

(define (quality-zero? pieces)
  (for/or ([p (in-list (cdr pieces))])
    (define trimmed (string-trim p))
    (and (>= (string-length trimmed) 2)
         (string-ci=? (substring trimmed 0 2) "q=")
         (let ([val (string->number (substring trimmed 2))]) (and val (zero? val))))))

(define (accepts-encoding? req encoding)
  (define enc-str (symbol->string encoding))
  (define accept-header
    (for/or ([h (in-list (request-headers/raw req))])
      (and (equal? (string-downcase (bytes->string/utf-8 (header-field h))) "accept-encoding")
           (header-value h))))
  (and accept-header
       (for/or ([part (in-list (regexp-split #rx"," (bytes->string/utf-8 accept-header)))])
         (define pieces (string-split part ";"))
         (define name (string-trim (car pieces)))
         (and (string-ci=? name enc-str) (not (quality-zero? pieces))))))
