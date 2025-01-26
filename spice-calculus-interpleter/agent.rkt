#lang racket
(require "../faster-minikanren/mk.rkt")
(require "public-define.rkt")
(require "mklist.rkt")
(require "value.rkt")
(require "term.rkt")
(require "process.rkt")
;#DEFINITION OF AGENT

;is a abstract?
(public-define (abstract-of a x q)
  (== a `((abstract ,x)  ,q)))
(public-define (abstract a)
  (fresh (x q) (abstract-of a x q) (variable x) (process q)))

;is a concrete?
(public-define (concrete-of a l m p)
  (== a `((concrete ,l ,m) ,p)))
(public-define (concrete a)
  (fresh (l m p) (concrete-of a l m p) (term m) (process p)))


;is a agent?
(public-define (agent a)
  (conde
   [(process a)]
   [(abstract a)]
   ))

;A=new(n)B
(public-define (agent-new B N A)
               (conde
                [(fresh(x p q)
                       (abstract-of A x p)
                       (new-of q N p)
                       (abstract-of B x q))]
                [(fresh (l m p)
                        (concrete-of A l m p)
                        (conde
                         [(concrete-of B `(,N . ,l) m p) (free-name-term m N)]
                         [(fresh (q) (new-of q N p) (concrete-of B l m q)) (not-free-name-term m N)])
                        )]
                [(new-of B N A) (process B)]))


(define (not-free-variable-process-all P l)
  (conde
   [(== l `())]
   [(fresh (head body) (== l `(,head . ,body))
           (not-free-variable-process P head)
           (not-free-variable-process-all P body))]))

;A=B|C
(public-define (agent-parallel A B C)
               (conde
                [(fresh (x c bc) (abstract-of C x c)
                        (parallel-of bc B c) (abstract-of A x bc)
                        (not-free-variable-process B x))]
                [(fresh (x b bc) (abstract-of B x b)
                        (parallel-of bc b C) (abstract-of A x bc)
                        (not-free-variable-process C x))]
                [(fresh (l m c bc) (concrete-of C l m c)
                        (parallel-of bc B c) (concrete-of A l m bc)
                        (not-free-variable-process-all B l)
                        )]
                [(fresh (l m b bc) (concrete-of B l m b)
                        (parallel-of bc b C) (concrete-of A l m bc)
                        (not-free-variable-process-all C l)
                        )]
                [(process B) (process C) (parallel-of A B C)]))