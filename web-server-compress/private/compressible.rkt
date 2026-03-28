#lang racket/base

(require racket/contract/base)

(provide (contract-out [compressible-mime-type? (-> (or/c bytes? #f) boolean?)]))

;; compressible types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define compressible-prefixes '(#"text/"))

(define compressible-types
  '(#"application/json" #"application/javascript"
                        #"application/xml"
                        #"application/xhtml+xml"
                        #"application/wasm"
                        #"application/ld+json"
                        #"application/graphql+json"
                        #"application/geo+json"
                        #"application/manifest+json"
                        #"application/rss+xml"
                        #"application/atom+xml"
                        #"image/svg+xml"))

;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bytes-downcase bs)
  (define out (make-bytes (bytes-length bs)))
  (for ([i (in-range (bytes-length bs))])
    (define b (bytes-ref bs i))
    (bytes-set! out
                i
                (if (<= 65 b 90)
                    (+ b 32)
                    b)))
  out)

(define (strip-params mime)
  (define idx
    (for/first ([i (in-range (bytes-length mime))]
                #:when (= (bytes-ref mime i) (char->integer #\;)))
      i))
  (if idx
      (subbytes mime 0 idx)
      mime))

(define (bytes-prefix? bs prefix)
  (and (>= (bytes-length bs) (bytes-length prefix))
       (equal? (subbytes bs 0 (bytes-length prefix)) prefix)))

;; predicate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compressible-mime-type? mime)
  (cond
    [(not mime) #t]
    [else
     (define base (bytes-downcase (strip-params mime)))
     (or (for/or ([prefix (in-list compressible-prefixes)])
           (bytes-prefix? base prefix))
         (and (member base compressible-types) #t))]))
