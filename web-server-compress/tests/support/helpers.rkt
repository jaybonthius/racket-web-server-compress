#lang racket/base

(require net/url
         racket/promise
         web-server/http/request-structs
         web-server/http/response-structs)

(provide make-req
         make-text-handler
         make-handler-with-mime
         response-header-value
         collect-response-body)

(define (make-req accept-encoding)
  (make-request #"GET"
                (url "http" #f "localhost" 8080 #t (list (path/param "test" '())) '() #f)
                (if accept-encoding
                    (list (make-header #"Accept-Encoding" (string->bytes/utf-8 accept-encoding)))
                    '())
                (delay
                  '())
                #f
                "127.0.0.1"
                8080
                "127.0.0.1"))

(define (make-text-handler text)
  (lambda (_req)
    (response 200 #"OK" (current-seconds) #"text/plain" '() (lambda (out) (write-string text out)))))

(define (make-handler-with-mime text mime)
  (lambda (_req)
    (response 200 #"OK" (current-seconds) mime '() (lambda (out) (write-string text out)))))

(define (response-header-value resp name)
  (for/or ([h (in-list (response-headers resp))])
    (and (equal? (header-field h) name) (header-value h))))

(define (collect-response-body resp)
  (define out (open-output-bytes))
  ((response-output resp) out)
  (get-output-bytes out))
