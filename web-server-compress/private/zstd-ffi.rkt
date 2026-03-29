#lang racket/base

(require (for-syntax racket/base)
         ffi/unsafe
         ffi/unsafe/define
         (only-in racket/contract/base integer-in)
         racket/runtime-path)

(provide open-zstd-output
         level/c)

(define-runtime-path libzstd.so '(so "libzstd"))
(define-ffi-definer define-zstd (ffi-lib libzstd.so))

(define-zstd ZSTD_isError (_fun _size -> _bool))
(define-zstd ZSTD_getErrorName (_fun _size -> _bytes/nul-terminated))

(define (check who code)
  (begin0 code
    (when (ZSTD_isError code)
      (error who (bytes->string/utf-8 (ZSTD_getErrorName code))))))

(define-zstd ZSTD_createCCtx (_fun -> _pointer))
(define-zstd ZSTD_freeCCtx (_fun _pointer -> _size))
(define-zstd ZSTD_CCtx_setParameter (_fun _pointer _int _int -> _size))

(define ZSTD_e_continue 0)
(define ZSTD_e_flush 1)
(define ZSTD_e_end 2)

;; ZSTD_c_compressionLevel = 100 per zstd.h
(define ZSTD_c_compressionLevel 100)

(define-zstd ZSTD_compressStream2 (_fun _pointer _pointer _pointer _int -> _size))

(define-zstd ZSTD_CStreamOutSize (_fun -> _size))
(define-zstd ZSTD_minCLevel (_fun -> _int))
(define-zstd ZSTD_maxCLevel (_fun -> _int))

(define level/c (integer-in (ZSTD_minCLevel) (ZSTD_maxCLevel)))

;; ZSTD_inBuffer/ZSTD_outBuffer: { void* ptr, size_t size, size_t pos }
;; sizeof(void*) == sizeof(size_t) on all Racket platforms, so struct = 3 * sizeof(size_t).
(define buffer-struct-size (* 3 (ctype-sizeof _size)))
(define default-out-buf-size (ZSTD_CStreamOutSize))

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

(define (buffer-set-input! buf src src-len)
  (ptr-set! buf _pointer 0 src)
  (ptr-set! buf _size 1 src-len)
  (ptr-set! buf _size 2 0))

(define (stream-compress! cctx end-op in-buf out-buf dst out-port)
  (let loop ()
    ;; Refresh out-buf pointer from dst in case GC relocated dst between calls
    (ptr-set! out-buf _pointer 0 dst)
    (buffer-reset! out-buf)
    (define remaining (check 'ZSTD_compressStream2 (ZSTD_compressStream2 cctx out-buf in-buf end-op)))
    (define written (buffer-pos out-buf))
    (when (> written 0)
      (write-bytes dst out-port 0 written))
    ;; flush/end: loop until remaining is 0
    ;; continue: loop until all input consumed
    (when (or (and (not (= end-op ZSTD_e_continue)) (> remaining 0))
              (and (= end-op ZSTD_e_continue) (< (buffer-pos in-buf) (ptr-ref in-buf _size 1))))
      (loop))))

(define (open-zstd-output out #:level [level 3] #:close? [close? #t] #:name [name 'zstd-output])
  (define cctx (ZSTD_createCCtx))
  (unless cctx
    (error 'open-zstd-output "failed to create compression context"))
  (check 'open-zstd-output (ZSTD_CCtx_setParameter cctx ZSTD_c_compressionLevel level))

  (define dst (make-bytes default-out-buf-size))
  (define out-buf (make-buffer dst default-out-buf-size))
  (define in-buf (make-buffer #f 0))
  (define closed? #f)

  (define (do-write bstr start end)
    (when closed?
      (error 'zstd-output "port is closed"))
    (define chunk (subbytes bstr start end))
    (buffer-set-input! in-buf chunk (bytes-length chunk))
    (stream-compress! cctx ZSTD_e_continue in-buf out-buf dst out)
    (- end start))

  (define (close)
    (unless closed?
      (set! closed? #t)
      (dynamic-wind void
                    (lambda ()
                      (buffer-set-input! in-buf #f 0)
                      (stream-compress! cctx ZSTD_e_end in-buf out-buf dst out)
                      (flush-output out))
                    (lambda ()
                      (ZSTD_freeCCtx cctx)
                      (when close?
                        (close-output-port out))))))

  (make-output-port name
                    out
                    (lambda (bstr start end non-block? breakable?) ;; review: ignore
                      (cond
                        [(= start end)
                         (buffer-set-input! in-buf #f 0)
                         (stream-compress! cctx ZSTD_e_flush in-buf out-buf dst out)
                         (flush-output out)
                         0]
                        [else (do-write bstr start end)]))
                    close
                    #f
                    #f
                    #f
                    #f
                    void
                    #f
                    (let ([mode 'block])
                      (case-lambda
                        [() mode]
                        [(new) (set! mode new)]))))
