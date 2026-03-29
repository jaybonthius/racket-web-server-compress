#lang racket/base

;; Test-only gzip compress/decompress utilities.
;; NOT part of the public API — used by tests and benchmarks for round-trip verification.

(require file/gzip
         file/gunzip)

(provide gzip-compress
         gzip-decompress)

(define (gzip-compress bstr)
  (define in (open-input-bytes bstr))
  (define out (open-output-bytes))
  (gzip-through-ports in out "data" 0)
  (get-output-bytes out))

(define (gzip-decompress bstr)
  (define in (open-input-bytes bstr))
  (define out (open-output-bytes))
  (gunzip-through-ports in out)
  (get-output-bytes out))
