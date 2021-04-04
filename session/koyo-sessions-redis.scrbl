#lang scribble/manual

@(require (for-label koyo
                     koyo/session
                     koyo/session/redis
                     racket/base
                     racket/string
                     redis))

@title{Redis session store for Koyo}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]
@defmodule[koyo/session/redis]

@(define (ss-tech description)
   (tech #:doc '(lib "koyo/scribblings/koyo.scrbl") description))

This module provides an implementation of a Koyo @ss-tech{session store}
that stores session data in a Redis database.

@defproc[(redis-session-store? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a Redis @ss-tech{session store}.
}

@defproc[(make-redis-session-store [pool redis-pool?]
                                   [#:prefix prefix non-empty-string? "sessions"]
                                   [#:ttl ttl exact-positive-integer? (* 7 86400)]) session-store?]{
  Returns a @ss-tech{session store} that stores session data in the
  Redis database that the clients provided by @racket[pool] are
  connected to.

  The @racket[#:prefix] argument can be used to configure the prefix
  of session entry keys inside of the database.  This can be useful
  when running multiple applications against the same Redis instance.

  The @racket[#:ttl] argument can be used to configure how long
  sessions are kept in the pool before they expire.  Every interaction
  with a session resets its expiration time.
}
