#lang racket/base

(require libbrotli
         racket/contract/base
         web-server/http
         "private/zstd-ffi.rkt"
         "private/gzip-ffi.rkt"
         "private/accept-encoding.rkt"
         "private/compressible.rkt")

(provide (contract-out [wrap-compress
                        (->* [(-> request? response?)]
                             [#:encodings (listof (or/c 'zstd 'br 'gzip))
                              #:compress? (-> response? boolean?)
                              #:zstd-level level/c
                              #:brotli-quality quality/c
                              #:brotli-window window/c
                              #:brotli-mode mode/c
                              #:gzip-level gzip-level/c]
                             (-> request? response?))]))

(define (wrap-compress handler
                       #:encodings [encodings '(zstd br gzip)]
                       #:compress? [compress? #f]
                       #:zstd-level [zstd-level 3]
                       #:brotli-quality [brotli-quality 5]
                       #:brotli-window [brotli-window 22]
                       #:brotli-mode [brotli-mode BROTLI_MODE_TEXT]
                       #:gzip-level [gzip-level 6])
  (define effective-compress?
    (or compress? (lambda (resp) (compressible-mime-type? (response-mime resp)))))
  (lambda (req)
    (define resp (handler req))
    (cond
      [(not (effective-compress? resp)) resp]
      [else
       (define selected
         (for/or ([enc (in-list encodings)])
           (and (accepts-encoding? req enc) enc)))
       (cond
         [(not selected) resp]
         [else
          (compress-response resp selected zstd-level brotli-quality brotli-window brotli-mode gzip-level)])])))

(define (compress-response resp encoding zstd-level brotli-quality brotli-window brotli-mode gzip-level)
  (define original-output (response-output resp))
  (define-values (content-encoding wrap-output)
    (case encoding
      [(zstd)
       (values #"zstd" (lambda (raw-out) (open-zstd-output raw-out #:level zstd-level #:close? #f)))]
      [(br)
       (values #"br"
               (lambda (raw-out)
                 (open-brotli-output raw-out
                                     #:quality brotli-quality
                                     #:window brotli-window
                                     #:mode brotli-mode
                                     #:close? #f)))]
      [(gzip)
       (values #"gzip" (lambda (raw-out) (open-gzip-output raw-out #:level gzip-level #:close? #f)))]
      [else (error 'wrap-compress "unsupported encoding: ~a" encoding)]))
  (struct-copy
   response
   resp
   [headers
    (list* (make-header #"Content-Encoding" content-encoding)
           (make-header #"Vary" #"Accept-Encoding")
           (response-headers resp))]
   [output
    (lambda (raw-out)
      (define out (wrap-output raw-out))
      (dynamic-wind void (lambda () (original-output out)) (lambda () (close-output-port out))))]))
