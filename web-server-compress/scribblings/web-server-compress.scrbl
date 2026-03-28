#lang scribble/doc

@(require scribble/manual
          (for-label web-server-compress
                     libbrotli
                     racket/base
                     racket/contract
                     web-server/http/request-structs
                     web-server/http/response-structs))

@title{Web Server Compress}

@author[(author+email "Jay Bonthius" "jay@jmbmail.com")]

@defmodule[web-server-compress]

Brotli compression middleware for the Racket
@link["https://docs.racket-lang.org/web-server/"]{web server}.

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

@codeblock|{
(require web-server-compress
         web-server/servlet-dispatch
         web-server/web-server)

(define (app req)
  (response/xexpr '(html (body "hello"))))

(serve
 #:dispatch (dispatch/servlet (wrap-brotli-compress app))
 #:port 8080)
}|

To override the default predicate, pass a custom @racket[compress?]:

@codeblock|{
;; Only compress JSON responses
(wrap-brotli-compress app
  #:compress? (lambda (resp)
                (equal? (response-mime resp) #"application/json")))
}|
}
