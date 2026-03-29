#lang racket/base

(require libbrotli
         rackunit
         web-server-compress/compress
         "support/helpers.rkt"
         "support/gzip.rkt"
         "support/zstd.rkt")

(module+ test
  (require rackunit/text-ui)
  (run-tests
   (test-suite "wrap-compress"

     (test-case "selects first priority encoding when client supports both"
       (define handler (wrap-compress (make-text-handler "hello") #:encodings '(zstd br)))
       (define resp (handler (make-req "zstd, br")))
       (check-equal? (response-header-value resp #"Content-Encoding") #"zstd"))

     (test-case "falls back to second encoding when client doesn't support first"
       (define handler (wrap-compress (make-text-handler "hello") #:encodings '(zstd br)))
       (define resp (handler (make-req "br, gzip")))
       (check-equal? (response-header-value resp #"Content-Encoding") #"br"))

     (test-case "no compression when client supports no listed encoding"
       (define handler (wrap-compress (make-text-handler "hello") #:encodings '(zstd br gzip)))
       (define resp (handler (make-req "identity")))
       (check-false (response-header-value resp #"Content-Encoding")))

     (test-case "no compression when Accept-Encoding absent"
       (define handler (wrap-compress (make-text-handler "hello") #:encodings '(zstd br)))
       (define resp (handler (make-req #f)))
       (check-false (response-header-value resp #"Content-Encoding")))

     (test-case "q=0 skips rejected encoding and falls back"
       (define handler (wrap-compress (make-text-handler "hello") #:encodings '(zstd br)))
       (define resp (handler (make-req "zstd;q=0, br")))
       (check-equal? (response-header-value resp #"Content-Encoding") #"br"))

     (test-case "q=0 on all encodings means no compression"
       (define handler (wrap-compress (make-text-handler "hello") #:encodings '(zstd br)))
       (define resp (handler (make-req "zstd;q=0, br;q=0")))
       (check-false (response-header-value resp #"Content-Encoding")))

     (test-case "single encoding in list"
       (define handler (wrap-compress (make-text-handler "hello") #:encodings '(zstd)))
       (define resp (handler (make-req "zstd")))
       (check-equal? (response-header-value resp #"Content-Encoding") #"zstd"))

     (test-case "custom priority order: br before zstd"
       (define handler (wrap-compress (make-text-handler "hello") #:encodings '(br zstd)))
       (define resp (handler (make-req "zstd, br")))
       (check-equal? (response-header-value resp #"Content-Encoding") #"br"))

     (test-case "zstd compression with custom level round-trips"
       (define text "hello with custom zstd level")
       (define handler (wrap-compress (make-text-handler text) #:encodings '(zstd) #:zstd-level 6))
       (define resp (handler (make-req "zstd")))
       (define decompressed (bytes->string/utf-8 (zstd-decompress (collect-response-body resp))))
       (check-equal? decompressed text))

     (test-case "brotli compression with custom quality round-trips"
       (define text "hello with custom brotli quality")
       (define handler (wrap-compress (make-text-handler text) #:encodings '(br) #:brotli-quality 8))
       (define resp (handler (make-req "br")))
       (define decompressed (bytes->string/utf-8 (brotli-decompress (collect-response-body resp))))
       (check-equal? decompressed text))

     (test-case "default parameters produce valid compressed output"
       (define text "default parameters test")
       (define handler (wrap-compress (make-text-handler text)))
       (define resp-zstd (handler (make-req "zstd, br")))
       (check-equal? (bytes->string/utf-8 (zstd-decompress (collect-response-body resp-zstd))) text)
       (define resp-br (handler (make-req "br")))
       (check-equal? (bytes->string/utf-8 (brotli-decompress (collect-response-body resp-br))) text))

     (test-case "custom #:compress? disables compression for all encodings"
       (define handler
         (wrap-compress (make-text-handler "hello")
                        #:encodings '(zstd br)
                        #:compress? (lambda (_resp) #f)))
       (define resp (handler (make-req "zstd, br")))
       (check-false (response-header-value resp #"Content-Encoding")))

     (test-case "non-compressible MIME type skips compression"
       (define handler
         (wrap-compress (make-handler-with-mime "fake" #"image/jpeg") #:encodings '(zstd br)))
       (define resp (handler (make-req "zstd, br")))
       (check-false (response-header-value resp #"Content-Encoding")))

     (test-case "Vary header present on zstd-compressed response"
       (define handler (wrap-compress (make-text-handler "hello") #:encodings '(zstd)))
       (define resp (handler (make-req "zstd")))
       (check-equal? (response-header-value resp #"Vary") #"Accept-Encoding"))

     (test-case "Vary header present on brotli-compressed response"
       (define handler (wrap-compress (make-text-handler "hello") #:encodings '(br)))
       (define resp (handler (make-req "br")))
       (check-equal? (response-header-value resp #"Vary") #"Accept-Encoding"))

     (test-case "no Vary header on uncompressed response"
       (define handler (wrap-compress (make-text-handler "hello") #:encodings '(zstd br gzip)))
       (define resp (handler (make-req "identity")))
       (check-false (response-header-value resp #"Vary")))

     (test-case "falls back to gzip when client lacks zstd and br"
       (define handler (wrap-compress (make-text-handler "hello") #:encodings '(zstd br gzip)))
       (define resp (handler (make-req "gzip")))
       (check-equal? (response-header-value resp #"Content-Encoding") #"gzip"))

     (test-case "selects gzip when only gzip in encodings list"
       (define handler (wrap-compress (make-text-handler "hello") #:encodings '(gzip)))
       (define resp (handler (make-req "gzip")))
       (check-equal? (response-header-value resp #"Content-Encoding") #"gzip"))

     (test-case "gzip round-trip through wrap-compress"
       (define text "gzip round-trip via wrap-compress")
       (define handler (wrap-compress (make-text-handler text) #:encodings '(gzip)))
       (define resp (handler (make-req "gzip")))
       (define decompressed (bytes->string/utf-8 (gzip-decompress (collect-response-body resp))))
       (check-equal? decompressed text))

     (test-case "custom #:gzip-level round-trips correctly"
       (define text "hello with custom gzip level")
       (define handler (wrap-compress (make-text-handler text) #:encodings '(gzip) #:gzip-level 9))
       (define resp (handler (make-req "gzip")))
       (define decompressed (bytes->string/utf-8 (gzip-decompress (collect-response-body resp))))
       (check-equal? decompressed text))

     (test-case "default three-encoding priority: zstd > br > gzip"
       (define handler (wrap-compress (make-text-handler "hello")))
       (define resp (handler (make-req "gzip, br, zstd")))
       (check-equal? (response-header-value resp #"Content-Encoding") #"zstd"))

     (test-case "Vary header present on gzip-compressed response"
       (define handler (wrap-compress (make-text-handler "hello") #:encodings '(gzip)))
       (define resp (handler (make-req "gzip")))
       (check-equal? (response-header-value resp #"Vary") #"Accept-Encoding"))

     (test-case "default parameters produce valid gzip output"
       (define text "default gzip test")
       (define handler (wrap-compress (make-text-handler text)))
       (define resp (handler (make-req "gzip")))
       (check-equal? (bytes->string/utf-8 (gzip-decompress (collect-response-body resp))) text)))))
