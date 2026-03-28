#lang racket/base

(require libbrotli
         racket/contract/base
         web-server/http
         "private/accept-encoding.rkt"
         "private/compressible.rkt")

(provide (contract-out
          [wrap-brotli-compress
           (->*
            [(-> request? response?)]
            [#:quality quality/c #:window window/c #:mode mode/c #:compress? (-> response? boolean?)]
            (-> request? response?))]))

(define (wrap-brotli-compress handler
                              #:quality [quality 5]
                              #:window [window 22]
                              #:mode [mode BROTLI_MODE_TEXT]
                              #:compress? [compress? #f])
  (define effective-compress?
    (or compress? (lambda (resp) (compressible-mime-type? (response-mime resp)))))
  (lambda (req)
    (define resp (handler req))
    (cond
      [(and (accepts-encoding? req 'br) (effective-compress? resp))
       (define original-output (response-output resp))
       (struct-copy
        response
        resp
        [headers
         (list* (make-header #"Content-Encoding" #"br")
                (make-header #"Vary" #"Accept-Encoding")
                (response-headers resp))]
        [output
         (lambda (raw-out)
           (define out
             (open-brotli-output raw-out #:quality quality #:window window #:mode mode #:close? #f))
           (original-output out)
           (close-output-port out))])]
      [else resp])))
