#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         koyo/random
         koyo/session
         racket/contract
         racket/format
         racket/match
         racket/port
         racket/serialize
         racket/string
         redis
         syntax/parse/define)

(provide
 redis-session-store?
 make-redis-session-store)

(struct redis-session-store
  (pool key-prefix ttl setter getter remover)
  #:methods gen:session-store
  [(define (session-store-generate-id! ss)
     (call-with-redis-client (redis-session-store-pool ss)
       (lambda (client)
         (define seq-id (redis-bytes-incr! client (fmt ss "seq")))
         (~a seq-id "." (generate-random-string)))))

   (define session-store-load! void)
   (define session-store-persist! void)

   (define (session-store-set! ss sid k v)
     (void (dispatch ss setter sid (symbol->bytes k) (serialize* v))))

   (define (session-store-ref ss sid k default)
     (match (dispatch ss getter sid (symbol->bytes k))
       ['(#f) (if (procedure? default) (default) default)]
       [`(,bs) (deserialize* bs)]))

   (define (session-store-update! ss sid k updater default)
     (define v (session-store-ref ss sid k default))
     (session-store-set! ss sid k (updater v)))

   (define (session-store-remove! ss sid k)
     (void (dispatch ss remover sid (symbol->bytes k))))])

(define-syntax-parser dispatch
  [(_ ss:expr meth:id sid:expr arg:expr ...)
   #:with accessor-id (format-id #'meth "redis-session-store-~a" #'meth)
   #'(dispatch* ss accessor-id sid arg ...)])

(define (dispatch* ss fn-accessor sid . args)
  (call-with-redis-client (redis-session-store-pool ss)
    (lambda (client)
      (define ttl (redis-session-store-ttl ss))
      (define fn (fn-accessor ss))
      (fn client
        #:keys (list (fmt-sid ss sid) sid)
        #:args (append args (list (number->string ttl)))))))

(define (serialize* v)
  (call-with-output-bytes
   (lambda (out)
     (write (serialize v) out))))

(define (deserialize* bs)
  (call-with-input-bytes bs
    (compose1 deserialize read)))

(define (fmt ss fmt-str . args)
  (define p (redis-session-store-key-prefix ss))
  (~a p ":" (apply format fmt-str args)))

(define (fmt-sid ss sid)
  (fmt ss "session:~a" sid))

(define (symbol->bytes s)
  (call-with-output-bytes
   (lambda (out)
     (write s out))))

(define/contract (make-redis-session-store
                  pool
                  #:prefix [key-prefix "sessions"]
                  #:ttl [ttl (* 7 86400)])
  (->* (redis-pool?)
       (#:prefix non-empty-string?
        #:ttl exact-positive-integer?)
       redis-session-store?)
  (define-values (setter getter remover)
    (call-with-redis-client pool
      (lambda (client)
        (values
         (make-redis-script client #<<SETTER
redis.call('HMSET', KEYS[1], ARGV[1], ARGV[2])
redis.call('EXPIRE', KEYS[1], ARGV[3])
SETTER
                            )
         (make-redis-script client #<<GETTER
local v = redis.call('HMGET', KEYS[1], ARGV[1])
redis.call('EXPIRE', KEYS[1], ARGV[2])
return v
GETTER
                            )
         (make-redis-script client #<<REMOVER
redis.call('HDEL', KEYS[1], ARGV[1])
redis.call('EXPIRE', KEYS[1], ARGV[2])
REMOVER
                            )))))

  (redis-session-store pool key-prefix ttl setter getter remover))
