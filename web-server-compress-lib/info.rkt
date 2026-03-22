#lang info
#|review: ignore|#

(define collection "web-server-compress")

(define version "0.1")

(define deps '("base" "web-server-lib" "libbrotli"))

(define build-deps '("rackunit-lib"))

(define pkg-desc "HTTP response compression middleware for the Racket web server")

(define pkg-authors '("Jay Bonthius"))

(define license 'MIT)
