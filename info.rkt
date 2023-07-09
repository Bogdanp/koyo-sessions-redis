#lang info

(define license 'BSD-3-Clause)
(define version "1.0")
(define collection "koyo")
(define deps '("base"
               "koyo-lib"
               "redis-lib"))
(define build-deps '("koyo-doc"
                     "racket-doc"
                     "redis-doc"
                     "rackunit-lib"
                     "scribble-lib"))
(define scribblings '(("session/koyo-sessions-redis.scrbl" () ("Web Development"))))
