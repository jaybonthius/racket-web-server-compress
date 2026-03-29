#lang scribble/doc

@(require scribble/manual
          (for-label web-server-compress
                     libbrotli
                     libz
                     racket/base
                     racket/contract
                     web-server/http/request-structs
                     web-server/http/response-structs
                     web-server/servlet-dispatch
                     web-server/web-server))

@title{Web Server Compress}

@author[(author+email "Jay Bonthius" "jay@jmbmail.com")]

@defmodule[web-server-compress]

HTTP response compression middleware for the Racket
@link["https://docs.racket-lang.org/web-server/"]{web server}. Supports
Brotli, zstd, and gzip compression, with priority-based encoding negotiation.

@section{Unified Compression}

@defthing[compressible-mime-type? (-> (or/c bytes? #f) boolean?)]{
Predicate that returns @racket[#t] for MIME types that benefit from compression:
all @tt{text/*} types, plus @tt{application/json}, @tt{application/javascript},
@tt{application/xml}, @tt{application/xhtml+xml}, @tt{application/wasm},
@tt{application/ld+json}, @tt{application/graphql+json}, @tt{application/geo+json},
@tt{application/manifest+json}, @tt{application/rss+xml}, @tt{application/atom+xml},
and @tt{image/svg+xml}. Returns @racket[#t] when the MIME type is @racket[#f]
(no Content-Type header). Already-compact formats like JPEG, PNG, and ZIP return
@racket[#f].}

@defproc[(wrap-compress [handler (-> request? response?)]
                        [#:encodings encodings (listof (or/c 'zstd 'br 'gzip)) '(zstd br gzip)]
                        [#:compress? compress? (-> response? boolean?)
                                     compressible-mime-type?]
                        [#:zstd-level zstd-level level/c 3]
                        [#:brotli-quality brotli-quality quality/c 5]
                        [#:brotli-window brotli-window window/c 22]
                        [#:brotli-mode brotli-mode mode/c BROTLI_MODE_TEXT]
                        [#:gzip-level gzip-level gzip-level/c 6])
         (-> request? response?)]{

Wraps @racket[handler] to compress responses using the best available encoding.
The server iterates through @racket[encodings] in order and selects the first
one the client accepts via @tt{Accept-Encoding}. If no encoding matches, the
response passes through uncompressed.

Supported encoding symbols: @racket['zstd], @racket['br], and @racket['gzip].

Compressed responses include @tt{Content-Encoding} and @tt{Vary: Accept-Encoding}
headers.

@racket[compress?] decides whether a given response should be compressed. The
default compresses text types and common structured formats (see below).

@racketblock[
(require web-server-compress
         web-server/servlet-dispatch
         web-server/web-server)

(define (app req)
  (response/xexpr '(html (body "hello"))))

(code:comment "Prefer zstd, fall back to brotli, then gzip")
(serve
 #:dispatch (dispatch/servlet
             (wrap-compress app #:encodings '(zstd br gzip)))
 #:port 8080)
]

To use only one encoding:

@racketblock[
(wrap-compress app #:encodings '(br) #:brotli-quality 9)
]
}

@section{Brotli Compression}

@defproc[(wrap-brotli-compress [handler (-> request? response?)]
                               [#:quality quality quality/c 5]
                               [#:window window window/c 22]
                               [#:mode mode mode/c BROTLI_MODE_TEXT]
                               [#:compress? compress? (-> response? boolean?)
                                            compressible-mime-type?])
         (-> request? response?)]{

Wraps @racket[handler] to compress responses with Brotli when the client
sends @tt{Accept-Encoding: br} and the response is compressible. Compressed
responses include @tt{Content-Encoding: br} and @tt{Vary: Accept-Encoding}
headers.

@racket[quality], @racket[window], and @racket[mode] control the Brotli
encoder. See @racketmodname[libbrotli] for details.

@racket[compress?] decides whether a given response should be compressed. The
default (@racket[compressible-mime-type?]) compresses text types
(@tt{text/*}) and common structured formats:

@itemlist[
  @item{@tt{application/json}, @tt{application/javascript}, @tt{application/xml}, @tt{application/xhtml+xml}}
  @item{@tt{application/wasm}, @tt{application/ld+json}, @tt{application/graphql+json}, @tt{application/geo+json}}
  @item{@tt{application/manifest+json}, @tt{application/rss+xml}, @tt{application/atom+xml}}
  @item{@tt{image/svg+xml}}
]

Responses with no @tt{Content-Type} header are also compressed. Already-compact
formats like JPEG, PNG, and ZIP are not compressed.

@racketblock[
(require web-server-compress
         web-server/servlet-dispatch
         web-server/web-server)

(define (app req)
  (response/xexpr '(html (body "hello"))))

(serve
 #:dispatch (dispatch/servlet (wrap-brotli-compress app))
 #:port 8080)
]

To override the default predicate, pass a custom @racket[compress?]:

@racketblock[
;; Only compress JSON responses
(wrap-brotli-compress app
  #:compress? (lambda (resp)
                (equal? (response-mime resp) #"application/json")))
]
}

@section{Zstd Compression}

@defthing[level/c flat-contract?]{
Contract for zstd compression levels. Accepts integers in the range supported
by the linked libzstd (typically @racket[-131072] to @racket[22]).}

@defproc[(wrap-zstd-compress [handler (-> request? response?)]
                             [#:level level level/c 3]
                             [#:compress? compress? (-> response? boolean?)
                                          compressible-mime-type?])
         (-> request? response?)]{

Wraps @racket[handler] to compress responses with zstd when the client
sends @tt{Accept-Encoding: zstd} and the response is compressible. Compressed
responses include @tt{Content-Encoding: zstd} and @tt{Vary: Accept-Encoding}
headers.

@racket[level] controls the zstd compression level. Higher values give better
compression at the cost of speed. The default of 3 is a good balance for
streaming use cases like Server-Sent Events.

@racket[compress?] works identically to @racket[wrap-brotli-compress]'s predicate.

@racketblock[
(serve
 #:dispatch (dispatch/servlet (wrap-zstd-compress app #:level 6))
 #:port 8080)
]
}

@section{Gzip Compression}

@defthing[gzip-level/c flat-contract?]{
Contract for gzip compression levels. Accepts integers from @racket[0]
(no compression) to @racket[9] (best compression).}

@defproc[(wrap-gzip-compress [handler (-> request? response?)]
                             [#:level level gzip-level/c 6]
                             [#:compress? compress? (-> response? boolean?)
                                          compressible-mime-type?])
         (-> request? response?)]{

Wraps @racket[handler] to compress responses with gzip when the client
sends @tt{Accept-Encoding: gzip} and the response is compressible. Compressed
responses include @tt{Content-Encoding: gzip} and @tt{Vary: Accept-Encoding}
headers.

@racket[level] controls the gzip compression level (0--9). Higher values give
better compression at the cost of speed. The default of 6 matches zlib's own
default and is a good general-purpose setting.

Gzip compression is provided by the @racketmodname[libz] package, which
bundles platform-specific zlib shared libraries.

@racket[compress?] works identically to @racket[wrap-brotli-compress]'s predicate.

@racketblock[
(serve
 #:dispatch (dispatch/servlet (wrap-gzip-compress app #:level 4))
 #:port 8080)
]
}
