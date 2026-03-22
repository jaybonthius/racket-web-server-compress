#lang info
#|review: ignore|#

(define collection "web-server-compress")

(define scribblings '(("scribblings/web-server-compress.scrbl")))

(define deps '("base"))

(define build-deps
  '("web-server-compress-lib" "libbrotli"
                              "scribble-lib"
                              "racket-doc"
                              "web-server-doc"
                              "web-server-lib"))

(define pkg-desc "documentation part of \"web-server-compress\"")

(define pkg-authors '("Jay Bonthius"))

(define license 'MIT)
