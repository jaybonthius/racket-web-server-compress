#lang racket/base

(require racket/contract/base
         web-server/http
         "private/gzip-ffi.rkt"
         "private/accept-encoding.rkt"
         "private/compressible.rkt")

(provide gzip-level/c
         (contract-out [wrap-gzip-compress
                        (->* [(-> request? response?)]
                             [#:level gzip-level/c #:compress? (-> response? boolean?)]
                             (-> request? response?))]))

(define (wrap-gzip-compress handler #:level [level 6] #:compress? [compress? #f])
  (define effective-compress?
    (or compress? (lambda (resp) (compressible-mime-type? (response-mime resp)))))
  (lambda (req)
    (define resp (handler req))
    (cond
      [(and (accepts-encoding? req 'gzip) (effective-compress? resp))
       (define original-output (response-output resp))
       (struct-copy response
                    resp
                    [headers
                     (list* (make-header #"Content-Encoding" #"gzip")
                            (make-header #"Vary" #"Accept-Encoding")
                            (response-headers resp))]
                    [output
                     (lambda (raw-out)
                       (define out (open-gzip-output raw-out #:level level #:close? #f))
                       (dynamic-wind void
                                     (lambda () (original-output out))
                                     (lambda () (close-output-port out))))])]
      [else resp])))
