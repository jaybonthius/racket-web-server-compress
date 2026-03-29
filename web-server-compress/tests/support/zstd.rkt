#lang racket/base

;; Test-only zstd compress/decompress utilities.
;; NOT part of the public API — used by tests and benchmarks for round-trip verification.

(require (for-syntax racket/base)
         ffi/unsafe
         ffi/unsafe/define
         racket/runtime-path)

(provide zstd-compress
         zstd-decompress)

(define-runtime-path libzstd.so '(so "libzstd"))
(define-ffi-definer define-zstd (ffi-lib libzstd.so))

(define-zstd ZSTD_isError (_fun _size -> _bool))
(define-zstd ZSTD_getErrorName (_fun _size -> _bytes/nul-terminated))

(define (check who code)
  (begin0 code
    (when (ZSTD_isError code)
      (error who (bytes->string/utf-8 (ZSTD_getErrorName code))))))

(define-zstd ZSTD_compress (_fun _bytes _size _bytes _size _int -> _size))
(define-zstd ZSTD_compressBound (_fun _size -> _size))
(define-zstd ZSTD_defaultCLevel (_fun -> _int))
(define default-compression-level (ZSTD_defaultCLevel))

(define (zstd-compress src [level default-compression-level])
  (define dst (make-bytes (ZSTD_compressBound (bytes-length src))))
  (define n
    (check 'zstd-compress (ZSTD_compress dst (bytes-length dst) src (bytes-length src) level)))
  (subbytes dst 0 n))

(define buffer-struct-size (* 3 (ctype-sizeof _size)))

(define (make-buffer ptr len)
  (define buf (malloc buffer-struct-size 'atomic-interior))
  (ptr-set! buf _pointer 0 ptr)
  (ptr-set! buf _size 1 len)
  (ptr-set! buf _size 2 0)
  buf)

(define (buffer-pos buf)
  (ptr-ref buf _size 2))

(define (buffer-reset! buf)
  (ptr-set! buf _size 2 0))

(define-zstd ZSTD_createDCtx (_fun -> _pointer))
(define-zstd ZSTD_freeDCtx (_fun _pointer -> _size))
(define-zstd ZSTD_decompressStream (_fun _pointer _pointer _pointer -> _size))
(define-zstd ZSTD_DStreamOutSize (_fun -> _size))

(define (zstd-decompress src)
  (define dctx (ZSTD_createDCtx))
  (unless dctx
    (error 'zstd-decompress "failed to create decompression context"))
  (dynamic-wind void
                (lambda ()
                  (define out-buf-size (ZSTD_DStreamOutSize))
                  (define dst (make-bytes out-buf-size))
                  (define in-buf (make-buffer src (bytes-length src)))
                  (define out-buf (make-buffer dst out-buf-size))
                  (define result-port (open-output-bytes))
                  (let loop ()
                    (buffer-reset! out-buf)
                    (define ret (check 'zstd-decompress (ZSTD_decompressStream dctx out-buf in-buf)))
                    (define written (buffer-pos out-buf))
                    (when (> written 0)
                      (write-bytes dst result-port 0 written))
                    (when (> ret 0)
                      (loop)))
                  (get-output-bytes result-port))
                (lambda () (ZSTD_freeDCtx dctx))))
