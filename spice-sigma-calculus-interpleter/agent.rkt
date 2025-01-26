#lang racket
(require "../faster-minikanren/mk.rkt")
(require "public-define.rkt")
(require "mklist.rkt")
(require "value.rkt")
(require "term.rkt")
(require "process.rkt")
(require "envproc.rkt")
;#DEFINITION OF AGENT

;is a abstract?
(public-define (abstract-of a x ad q)
  (== a `((abstract ,x ,ad)  ,q)))
(public-define (abstract a)
  (fresh (x ad q) (abstract-of a x ad q) (variable x) (envproc q)))

;is a concrete?
(public-define (concrete-of a l m p)
  (== a `((concrete ,l ,m) ,p)))
(public-define (concrete a)
  (fresh (l m p) (concrete-of a l m p) (term m) (envproc p)))


;is a concrete?
(public-define (process-agent-of a p)
  (== a `(process ,p)))
(public-define (process-agent a)
  (fresh (p) (process-agent-of a p)(envproc p)))


;is a agent?
(public-define (agent a)
  (conde
   [(envproc a)]
   [(process-agent a)]
   [(abstract a)]
   ))


;A=new(n)B
(public-define (agent-new B N A)
               (conde
                [(fresh(x ad p q)
                       (abstract-of A x ad p)
                       (new-envproc-of q N p)
                       (abstract-of B x ad q))]
                [(fresh (l m ad p)
                        (concrete-of A l m p)
                        (conde
                         [(concrete-of B `(,N . ,l) m p) (free-name-value m N)]
                         [(fresh (q) (new-envproc-of q N p) (concrete-of B l m q)) (not-free-name-value m N)])
                        )]
                [(fresh (p q)
                        (process-agent-of B q)
                        (process-agent-of A p)
                        (new-envproc-of q N p))]))


(define (not-free-name-envproc-all P l)
  (conde
   [(== l `())]
   [(fresh (head body) (== l `(,head . ,body))
           (not-free-name-envproc P head)
           (not-free-name-envproc-all P body))]))

;A=B|C
(public-define (agent-parallel A B C)
               (conde
                [(fresh (x c bc ad) (abstract-of C x ad c)
                        (parallel-envproc-of bc B c) (abstract-of A x `(() ,ad) bc))]
                [(fresh (x b bc ad) (abstract-of B x ad b)
                        (parallel-envproc-of bc b C) (abstract-of A x `(,ad ()) bc))]
                [(fresh (l m c bc) (concrete-of C l m c)
                        (parallel-envproc-of bc B c) (concrete-of A l m bc)
                        (not-free-name-envproc-all B l)
                        )]
                [(fresh (l m b bc) (concrete-of B l m b)
                        (parallel-envproc-of bc b C) (concrete-of A l m bc)
                        (not-free-name-envproc-all C l)
                        )]
                [(envproc B) (envproc C) (parallel-envproc-of A B C)]))