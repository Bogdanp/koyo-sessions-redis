#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         koyo/random
         koyo/session
         racket/contract
         racket/fasl
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
  (pool key-prefix ttl incrementer setter getter remover)
  #:methods gen:session-store
  [(define (session-store-generate-id! ss)
     (call-with-redis-client (redis-session-store-pool ss)
       (lambda (client)
         (define incr (redis-session-store-incrementer ss))
         (define seq-id
           (incr client
                 #:keys (list (fmt ss "seq"))
                 #:args null))
         (~a seq-id "." (generate-random-string)))))

   (define session-store-load! void)
   (define session-store-persist! void)

   (define (session-store-ref ss sid k default)
     (match (dispatch ss getter sid (symbol->bytes k))
       ['(#f) (if (procedure? default) (default) default)]
       [`(,bs) (deserialize* bs)]))

   (define (session-store-set! ss sid k v)
     (void (dispatch ss setter sid (symbol->bytes k) (serialize* v))))

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
     (s-exp->fasl (serialize v) out))))

(define (deserialize* bs)
  (call-with-input-bytes bs
    (compose1 deserialize fasl->s-exp)))

(define (fmt ss . args)
  (define p (redis-session-store-key-prefix ss))
  (call-with-output-bytes
   (lambda (out)
     (write-bytes p out)
     (write-bytes #":" out)
     (for ([arg (in-list args)])
       (write-string arg out)))))

(define (fmt-sid ss sid)
  (fmt ss "session:" sid))

(define (symbol->bytes s)
  (call-with-output-bytes
   (lambda (out)
     (write-string (symbol->string s) out))))

(define/contract (make-redis-session-store
                  pool
                  #:prefix [key-prefix "sessions"]
                  #:ttl [ttl (* 7 86400)])
  (->* (redis-pool?)
       (#:prefix non-empty-string?
        #:ttl exact-positive-integer?)
       session-store?)
  (define-values (incrementer setter getter remover)
    (call-with-redis-client pool
      (lambda (client)
        (values
         (make-redis-script client #<<INCREMENTER
local seq = 1
local function incr()
  seq = redis.call('INCR', KEYS[1])
end

if not pcall(incr) then
  redis.call('SET', KEYS[1], '1')
end
return seq
INCREMENTER
                            )
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

  (define prefix-bs (string->bytes/utf-8 key-prefix))
  (redis-session-store pool prefix-bs ttl incrementer setter getter remover))

(module+ test
  (require rackunit)

  (serializable-struct foo (x)
    #:transparent)

  (define url (getenv "REDIS_URL"))
  (when url
    (define-values (username password host port db)
      (parse-redis-url url))
    (define pool
      (make-redis-pool
       #:username username
       #:password password
       #:host host
       #:port port
       #:db db))
    (define store
      (make-redis-session-store pool))
    (call-with-redis-client pool redis-flush-all!)

    (define sid (session-store-generate-id! store))

    (test-case "ref nonexistent"
      (check-equal? (session-store-ref store sid 'a 'not-found) 'not-found)
      (check-exn
       #rx"not found"
       (lambda ()
         (session-store-ref store sid 'a (λ () (error "not found"))))))

    (test-case "store and retrieve"
      (session-store-set! store sid 'a '(1 2 3))
      (check-equal? (session-store-ref store sid 'a #f) '(1 2 3))
      (session-store-remove! store sid 'a)
      (check-false (session-store-ref store sid 'a #f)))

    (test-case "update"
      (session-store-update! store sid 'x add1 0)
      (check-equal? (session-store-ref store sid 'x #f) 1)
      (session-store-update! store sid 'x add1 0)
      (check-equal? (session-store-ref store sid 'x #f) 2))

    (test-case "store serializable struct"
      (session-store-set! store sid 'foo (foo 42))
      (check-equal? (session-store-ref store sid 'foo #f) (foo 42)))

    (test-case "handle seq overflow"
      (call-with-redis-client pool
        (lambda (c)
          (redis-bytes-set! c #"sessions:seq" (number->string (sub1 (expt 2 63))))))
      (check-true
       (string-prefix? (session-store-generate-id! store) "1.")))))
