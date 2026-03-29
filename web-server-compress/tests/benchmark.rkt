#lang racket/base

(require libbrotli
         libz
         racket/format
         racket/string
         "support/zstd.rkt")

;; benchmark: compare zstd vs brotli vs gzip compression on representative payloads

(define (make-json-payload n)
  (string->bytes/utf-8
   (string-append
    "["
    (string-join
     (for/list ([i (in-range n)])
       (~a "{\"id\":" i ",\"name\":\"item " i "\",\"value\":" (* i 3.14) ",\"active\":true}"))
     ",")
    "]")))

(define (make-html-payload n)
  (string->bytes/utf-8 (string-append "<!DOCTYPE html><html><head><title>Test</title></head><body>"
                                      (apply string-append
                                             (for/list ([i (in-range n)])
                                               (~a "<div class=\"item\" id=\"item-"
                                                   i
                                                   "\">"
                                                   "<h2>Item "
                                                   i
                                                   "</h2>"
                                                   "<p>This is the description for item number "
                                                   i
                                                   ".</p>"
                                                   "</div>")))
                                      "</body></html>")))

(define (make-sse-payload n)
  (string->bytes/utf-8 (apply string-append
                              (for/list ([i (in-range n)])
                                (~a "event: datastar-patch-signals\n"
                                    "data: signals {\"count\":"
                                    i
                                    ",\"message\":\"update "
                                    i
                                    "\"}\n\n")))))

(define (benchmark-compress name compress-fn payload iterations)
  (collect-garbage)
  (collect-garbage)
  (define start (current-inexact-milliseconds))
  (define compressed-size 0)
  (for ([_ (in-range iterations)])
    (define result (compress-fn payload))
    (set! compressed-size (bytes-length result)))
  (define elapsed (- (current-inexact-milliseconds) start))
  (define original-size (bytes-length payload))
  (define ratio (* 100.0 (/ compressed-size original-size)))
  (printf "  ~a: ~a ms (~a iterations), ratio: ~a% (~a -> ~a bytes)\n"
          name
          (~r elapsed #:precision 1)
          iterations
          (~r ratio #:precision 1)
          original-size
          compressed-size))

(define (run-benchmarks)
  (define payloads
    `(("JSON (small, 1KB)" ,(make-json-payload 10) 1000)
      ("JSON (medium, 50KB)" ,(make-json-payload 500) 200)
      ("HTML (small, 2KB)" ,(make-html-payload 10) 1000)
      ("HTML (medium, 50KB)" ,(make-html-payload 500) 200)
      ("SSE events (small, 1KB)" ,(make-sse-payload 10) 1000)
      ("SSE events (medium, 50KB)" ,(make-sse-payload 500) 200)))

  (printf "=== Compression Benchmark: zstd vs brotli vs gzip ===\n\n")

  (for ([entry (in-list payloads)])
    (define label (car entry))
    (define payload (cadr entry))
    (define iters (caddr entry))
    (printf "~a (~a bytes):\n" label (bytes-length payload))
    (benchmark-compress "zstd level 1" (lambda (p) (zstd-compress p 1)) payload iters)
    (benchmark-compress "zstd level 3" (lambda (p) (zstd-compress p 3)) payload iters)
    (benchmark-compress "zstd level 6" (lambda (p) (zstd-compress p 6)) payload iters)
    (benchmark-compress "brotli quality 1" (lambda (p) (brotli-compress p 1)) payload iters)
    (benchmark-compress "brotli quality 5" (lambda (p) (brotli-compress p 5)) payload iters)
    (benchmark-compress "brotli quality 9" (lambda (p) (brotli-compress p 9)) payload iters)
    (benchmark-compress "gzip level 1" (lambda (p) (gzip-compress p 1)) payload iters)
    (benchmark-compress "gzip level 6" (lambda (p) (gzip-compress p 6)) payload iters)
    (benchmark-compress "gzip level 9" (lambda (p) (gzip-compress p 9)) payload iters)
    (newline)))

(module+ main
  (run-benchmarks))
