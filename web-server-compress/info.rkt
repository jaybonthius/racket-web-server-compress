#lang info
#|review: ignore|#

(define collection 'multi)

(define deps '("web-server-compress-lib" "web-server-compress-doc"))

(define implies '("web-server-compress-lib" "web-server-compress-doc"))

(define pkg-desc "HTTP response compression middleware for the Racket web server")

(define pkg-authors '("Jay Bonthius"))

(define license 'MIT)
