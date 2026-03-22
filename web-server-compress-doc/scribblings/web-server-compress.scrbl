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

Returns a new handler that compresses responses with Brotli when the client
sends @tt{Accept-Encoding: br} and the response is deemed compressible.
Compressed responses include @tt{Content-Encoding: br} and
@tt{Vary: Accept-Encoding} headers. When compression does not apply, the
response passes through unchanged.

@racket[quality], @racket[window], and @racket[mode] control the Brotli
encoder --- see @racketmodname[libbrotli] for details.

@racket[compress?] overrides the built-in predicate that decides whether a
response should be compressed. The default compresses text types
(@tt{text/*}) and common structured formats like @tt{application/json},
@tt{application/xml}, @tt{image/svg+xml}, and others.

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
}
