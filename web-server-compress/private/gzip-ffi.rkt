#lang racket/base

(require (for-syntax racket/base)
         ffi/unsafe
         ffi/unsafe/define
         (only-in racket/contract/base integer-in)
         racket/runtime-path)

(provide open-gzip-output
         gzip-level/c)

(define-runtime-path libz.so '(so "libz"))
(define-ffi-definer define-z
  (ffi-lib libz.so #:fail (lambda () (ffi-lib "zlib1" #:fail (lambda () (ffi-lib "libz"))))))

;; --- Constants ---

(define Z_OK 0)
(define Z_STREAM_END 1)
(define Z_BUF_ERROR -5)

(define Z_NO_FLUSH 0)
(define Z_SYNC_FLUSH 2)
(define Z_FINISH 4)

(define Z_DEFLATED 8)
(define Z_DEFAULT_STRATEGY 0)

;; windowBits = 15 + 16 = 31 produces gzip format (the +16 tells zlib to emit gzip headers)
(define GZIP_WINDOW_BITS 31)
(define DEFAULT_MEM_LEVEL 8)

(define gzip-level/c (integer-in 0 9))

;; --- z_stream struct layout (LP64) ---
;; We only need 4 fields: next_in (offset 0), avail_in (offset 8),
;; next_out (offset 24), avail_out (offset 32).
;; Total struct size is 112 bytes on LP64 (Linux/macOS 64-bit).
;; On Windows LLP64, unsigned long is 4 bytes and offsets differ — guard against this.

(define z-stream-size 112)

(unless (= (ctype-sizeof _pointer) 8)
  (error 'gzip-ffi "unsupported platform: requires 64-bit (LP64) architecture"))

(define (make-z-stream)
  (define s (malloc z-stream-size 'atomic-interior))
  (memset s 0 z-stream-size)
  s)

(define (z-stream-set-input! strm ptr len)
  (ptr-set! strm _pointer 'abs 0 ptr)       ;; next_in at byte offset 0
  (ptr-set! strm _uint 'abs 8 len))         ;; avail_in at byte offset 8

(define (z-stream-avail-in strm)
  (ptr-ref strm _uint 'abs 8))              ;; avail_in at byte offset 8

(define (z-stream-set-output! strm ptr len)
  (ptr-set! strm _pointer 'abs 24 ptr)      ;; next_out at byte offset 24
  (ptr-set! strm _uint 'abs 32 len))        ;; avail_out at byte offset 32

(define (z-stream-avail-out strm)
  (ptr-ref strm _uint 'abs 32))             ;; avail_out at byte offset 32

;; --- FFI bindings ---

(define-z zlibVersion (_fun -> _string))

;; deflateInit2 is a C macro that expands to deflateInit2_ with version and struct size
(define-z deflateInit2_
  (_fun _pointer _int _int _int _int _int _string _int -> _int))

(define-z deflate_
  (_fun _pointer _int -> _int)
  #:c-id deflate)

(define-z deflateEnd_
  (_fun _pointer -> _int)
  #:c-id deflateEnd)

;; --- Error checking ---

(define (check who code)
  (begin0 code
    (when (and (< code 0) (not (= code Z_BUF_ERROR)))
      (error who "zlib error code ~a" code))))

;; --- Output buffer ---

(define default-out-buf-size 16384)

;; --- Streaming compression ---

(define (stream-deflate! strm flush dst dst-size out-port)
  (let loop ()
    ;; Refresh next_out from dst in case GC relocated it
    (z-stream-set-output! strm dst dst-size)
    (define ret (check 'deflate (deflate_ strm flush)))
    (define written (- dst-size (z-stream-avail-out strm)))
    (when (> written 0)
      (write-bytes dst out-port 0 written))
    ;; For Z_NO_FLUSH: loop until all input consumed
    ;; For Z_SYNC_FLUSH/Z_FINISH: loop until output buffer not full
    (cond
      [(= flush Z_NO_FLUSH)
       (when (> (z-stream-avail-in strm) 0)
         (loop))]
      [(= flush Z_FINISH)
       (when (not (= ret Z_STREAM_END))
         (loop))]
      [else ;; Z_SYNC_FLUSH
       (when (= (z-stream-avail-out strm) 0)
         (loop))])))

;; --- Public API ---

(define (open-gzip-output out #:level [level 6] #:close? [close? #t] #:name [name 'gzip-output])
  (define strm (make-z-stream))
  (define ret
    (deflateInit2_ strm level Z_DEFLATED GZIP_WINDOW_BITS DEFAULT_MEM_LEVEL
                   Z_DEFAULT_STRATEGY (zlibVersion) z-stream-size))
  (unless (= ret Z_OK)
    (error 'open-gzip-output "deflateInit2 failed with code ~a" ret))

  (define dst (make-bytes default-out-buf-size))
  (define closed? #f)

  (define (do-write bstr start end)
    (when closed?
      (error 'gzip-output "port is closed"))
    (define chunk (subbytes bstr start end))
    (z-stream-set-input! strm chunk (bytes-length chunk))
    (stream-deflate! strm Z_NO_FLUSH dst default-out-buf-size out)
    (- end start))

  (define (close)
    (unless closed?
      (set! closed? #t)
      (dynamic-wind void
                    (lambda ()
                      (z-stream-set-input! strm #f 0)
                      (stream-deflate! strm Z_FINISH dst default-out-buf-size out)
                      (flush-output out))
                    (lambda ()
                      (deflateEnd_ strm)
                      (when close?
                        (close-output-port out))))))

  (make-output-port name
                    out
                    (lambda (bstr start end non-block? breakable?) ;; review: ignore
                      (cond
                        [(= start end)
                         (z-stream-set-input! strm #f 0)
                         (stream-deflate! strm Z_SYNC_FLUSH dst default-out-buf-size out)
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
