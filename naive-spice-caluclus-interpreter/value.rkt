#lang racket
(require "../faster-minikanren/mk.rkt")
(require "public-define.rkt")
(require "mklist.rkt")

;#DEFINITION OF VALUE
;is x name?
(public-define (name-of x n)(== x `(name ,n)))
(public-define (name x)
  (fresh (n) (name-of x n)))

;is x variable?
(public-define (variable-of x v)(== x `(variable ,v)))
(public-define (variable x)
  (fresh (v) (variable-of x v)))
(public-define (variable-list l)
  (conde
   [(== l null)]
   [(fresh (head body) (== l `(,head . ,body)) (variable head) (variable-list body))]
   ))

;is x constant?
(public-define (constant-of x c) (== x `(constant ,c)))
(public-define (constant x)
               (fresh (v) (constant-of x v)))


;is x hash value?
(public-define (hash-value-of x v) (== x `(hash-value ,v)))
(public-define (hash-value x)
  (fresh (v) (hash-value-of x v) (value v)))

;is x pair value?
(public-define (pair-value-of x u v) (== x `(pair-value ,u ,v)))
(public-define (pair-value x)
               (fresh (u v) (pair-value-of x u v) (value u) (value v)))

(public-define nil-value `nil-value)




;is x value?
(public-define (value x)
  (conde
   [(constant x)]
   [(== x nil-value)]
   [(name x)]
   [(variable x)]
   [(pair-value x)]
   [(hash-value x)]
   ))

;X=Y?
(public-define (value-eq X Y)
               (== X Y))

;X!=Y?
(public-define (value-neq X Y)
               (=/= X Y))




;FREE NAME
;N in fn(V)
(public-define (free-name-value V N)
               (conde
                [(name V) (== V N)]
                [(fresh (u v) (pair-value-of V u v) (conde
                                                     [(free-name-value u N)] [(free-name-value v N)]))]
                [(fresh (v) (hash-value-of V v) (free-name-value v N))]))
;N is not in fn(V)
(public-define (not-free-name-value V N)
               (conde
                [(name V) (=/= V N)]
                [(variable V)]
                [(constant V)]
                [(fresh (u v) (pair-value-of V u v) (not-free-name-value u N) (not-free-name-value v N))]
                [(== nil-value V)]
                [(fresh (v) (hash-value-of V v) (not-free-name-value v N))]))

;FREE VARIABLE
;is v in fv(U)?
(public-define (free-variable-value U v)
               (conde
                [(variable U) (== U v)]
                [(fresh (u1 u2) (pair-value-of U u1 u2)
                        (conde
                         [(free-variable-value u1 v)]
                         [(free-variable-value u2 v)]))]
                [(fresh (u) (hash-value-of U u) (free-variable-value u v))]))


(public-define (not-free-variable-value U v)
               (conde
                [(variable U) (== U v)]
                [(name U)]
                [(constant U)]
                [(== U nil-value)]
                [(fresh (u1 u2) (pair-value-of U u1 u2)(not-free-variable-value u1 v)(not-free-variable-value u2 v))]
                [(fresh (u) (hash-value-of U u) (not-free-variable-value u v))]))



;#SET OF FREE VALUE

;is fv(U) subset of V?
(public-define (super-free-variable-value U V)
               (conde
                [(variable U)(contains V U)]
                [(name U)]
                [(constant U)]
                [(== U nil-value)]
                [(fresh (u1 u2) (pair-value-of U u1 u2) (super-free-variable-value u1 V) (super-free-variable-value u2 V))]
                [(fresh (u) (hash-value-of U u) (super-free-variable-value u V))]))


