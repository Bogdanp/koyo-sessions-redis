#lang info

(define version "0.0")
(define collection "koyo")
(define deps '("base"
               "koyo-lib"
               "redis-lib"))
(define build-deps '("koyo-doc"
                     "racket-doc"
                     "redis-doc"
                     "rackunit-lib"
                     "scribble-lib"))
(define scribblings '(("session/koyo-sessions-redis.scrbl" ())))
