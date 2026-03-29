#lang info
#|review: ignore|#

(define collection "web-server-compress")

(define version "0.2")

(define deps '("base" "web-server-lib" "libbrotli" "libzstd"))

(define build-deps '("rackunit-lib" "scribble-lib" "racket-doc" "web-server-doc"))

(define scribblings '(("scribblings/web-server-compress.scrbl")))

(define pkg-desc "HTTP response compression middleware for the Racket web server")

(define pkg-authors '("Jay Bonthius"))

(define license 'MIT)
