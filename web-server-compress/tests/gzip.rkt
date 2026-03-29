#lang racket/base

(require racket/string
         rackunit
         web-server-compress/gzip
         web-server/http/response-structs
         "support/helpers.rkt"
         "support/gzip.rkt")

(define (collect-response-body/decompressed resp)
  (gzip-decompress (collect-response-body resp)))

(module+ test
  (require rackunit/text-ui)
  (run-tests
   (test-suite "wrap-gzip-compress"

     (test-case "adds Content-Encoding: gzip when client accepts gzip"
       (define handler (wrap-gzip-compress (make-text-handler "hello")))
       (define resp (handler (make-req "gzip, deflate")))
       (check-equal? (response-header-value resp #"Content-Encoding") #"gzip"))

     (test-case "no Content-Encoding when client does not accept gzip"
       (define handler (wrap-gzip-compress (make-text-handler "hello")))
       (define resp (handler (make-req "br, zstd")))
       (check-false (response-header-value resp #"Content-Encoding")))

     (test-case "no Content-Encoding when Accept-Encoding header absent"
       (define handler (wrap-gzip-compress (make-text-handler "hello")))
       (define resp (handler (make-req #f)))
       (check-false (response-header-value resp #"Content-Encoding")))

     (test-case "quality weights are stripped correctly"
       (define handler (wrap-gzip-compress (make-text-handler "hello")))
       (define resp (handler (make-req "br;q=1.0, gzip;q=0.5")))
       (check-equal? (response-header-value resp #"Content-Encoding") #"gzip"))

     (test-case "adds Vary: Accept-Encoding when compressed"
       (define handler (wrap-gzip-compress (make-text-handler "hello")))
       (define resp (handler (make-req "gzip")))
       (check-equal? (response-header-value resp #"Vary") #"Accept-Encoding"))

     (test-case "no Vary header when not compressed"
       (define handler (wrap-gzip-compress (make-text-handler "hello")))
       (define resp (handler (make-req "br")))
       (check-false (response-header-value resp #"Vary")))

     (test-case "round-trip: compressed body decompresses to original"
       (define text "hello, gzip world!")
       (define handler (wrap-gzip-compress (make-text-handler text)))
       (define resp (handler (make-req "gzip")))
       (define decompressed (bytes->string/utf-8 (collect-response-body/decompressed resp)))
       (check-equal? decompressed text))

     (test-case "uncompressed path: body is passed through unchanged"
       (define text "plain text response")
       (define handler (wrap-gzip-compress (make-text-handler text)))
       (define resp (handler (make-req "br")))
       (define body (bytes->string/utf-8 (collect-response-body resp)))
       (check-equal? body text))

     (test-case "custom #:level round-trips correctly"
       (define text "hello with custom gzip level")
       (define handler (wrap-gzip-compress (make-text-handler text) #:level 9))
       (define resp (handler (make-req "gzip")))
       (define decompressed (bytes->string/utf-8 (collect-response-body/decompressed resp)))
       (check-equal? decompressed text))

     (test-case "q=0 rejects encoding"
       (define handler (wrap-gzip-compress (make-text-handler "hello")))
       (define resp (handler (make-req "gzip;q=0, br")))
       (check-false (response-header-value resp #"Content-Encoding")))

     (test-case "mid-stream flush produces decodable output (SSE scenario)"
       (define events
         '("event: datastar-patch-elements\ndata: elements <div>hello</div>\n\n"
           "event: datastar-patch-signals\ndata: signals {\"count\":1}\n\n"))
       (define handler
         (wrap-gzip-compress (lambda (_req)
                               (response 200
                                         #"OK"
                                         (current-seconds)
                                         #"text/event-stream"
                                         '()
                                         (lambda (out)
                                           (for ([ev (in-list events)])
                                             (write-string ev out)
                                             (flush-output out)))))))
       (define resp (handler (make-req "gzip")))
       (define compressed (collect-response-body resp))
       (define result (bytes->string/utf-8 (gzip-decompress compressed)))
       (check-true (string-contains? result "datastar-patch-elements"))
       (check-true (string-contains? result "datastar-patch-signals"))
       (check-true (string-contains? result "<div>hello</div>"))
       (check-true (string-contains? result "{\"count\":1}")))

     (test-case "compresses text/html responses"
       (define handler (wrap-gzip-compress (make-handler-with-mime "hello" #"text/html")))
       (define resp (handler (make-req "gzip")))
       (check-equal? (response-header-value resp #"Content-Encoding") #"gzip"))

     (test-case "compresses application/json responses"
       (define handler (wrap-gzip-compress (make-handler-with-mime "{}" #"application/json")))
       (define resp (handler (make-req "gzip")))
       (check-equal? (response-header-value resp #"Content-Encoding") #"gzip"))

     (test-case "compresses text/event-stream responses"
       (define handler
         (wrap-gzip-compress (make-handler-with-mime "data: hi\n\n" #"text/event-stream")))
       (define resp (handler (make-req "gzip")))
       (check-equal? (response-header-value resp #"Content-Encoding") #"gzip"))

     (test-case "does not compress image/jpeg responses"
       (define handler (wrap-gzip-compress (make-handler-with-mime "fake" #"image/jpeg")))
       (define resp (handler (make-req "gzip")))
       (check-false (response-header-value resp #"Content-Encoding")))

     (test-case "does not compress image/png responses"
       (define handler (wrap-gzip-compress (make-handler-with-mime "fake" #"image/png")))
       (define resp (handler (make-req "gzip")))
       (check-false (response-header-value resp #"Content-Encoding")))

     (test-case "does not compress application/zip responses"
       (define handler (wrap-gzip-compress (make-handler-with-mime "fake" #"application/zip")))
       (define resp (handler (make-req "gzip")))
       (check-false (response-header-value resp #"Content-Encoding")))

     (test-case "compresses responses with no Content-Type"
       (define handler (wrap-gzip-compress (make-handler-with-mime "hello" #f)))
       (define resp (handler (make-req "gzip")))
       (check-equal? (response-header-value resp #"Content-Encoding") #"gzip"))

     (test-case "compresses text/html with charset parameter"
       (define handler
         (wrap-gzip-compress (make-handler-with-mime "hello" #"text/html; charset=utf-8")))
       (define resp (handler (make-req "gzip")))
       (check-equal? (response-header-value resp #"Content-Encoding") #"gzip"))

     (test-case "custom #:compress? can disable compression"
       (define handler
         (wrap-gzip-compress (make-text-handler "hello") #:compress? (lambda (_resp) #f)))
       (define resp (handler (make-req "gzip")))
       (check-false (response-header-value resp #"Content-Encoding")))

     (test-case "custom #:compress? can enable compression for normally-skipped types"
       (define handler
         (wrap-gzip-compress (make-handler-with-mime "fake" #"image/jpeg")
                             #:compress? (lambda (_resp) #t)))
       (define resp (handler (make-req "gzip")))
       (check-equal? (response-header-value resp #"Content-Encoding") #"gzip"))

     (test-case "mime type matching is case-insensitive"
       (define handler (wrap-gzip-compress (make-handler-with-mime "hello" #"Text/HTML")))
       (define resp (handler (make-req "gzip")))
       (check-equal? (response-header-value resp #"Content-Encoding") #"gzip")))))
