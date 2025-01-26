#lang racket
(require "../faster-minikanren/mk.rkt")
(require "public-define.rkt")
(require "mklist.rkt")
(require "value.rkt")


;#DEFINITION OF TERM

;is x variable?
(public-define (variable-of x v)(== x `(variable ,v)))
(public-define (variable x)
  (fresh (v) (variable-of x v)))
(public-define (variable-list l)
  (conde
   [(== l null)]
   [(fresh (head body) (== l `(,head . ,body)) (variable head) (variable-list body))]
   ))

;is x hash-expr
(public-define (hash-expr-of x y)
  (== x `(hash-expr ,y)))
(public-define (hash-expr x)
  (fresh (y) (hash-expr-of x y) (term y)))

;is x pair
(public-define (pair-term-of x y z)
  (== x `(pair-term ,y ,z)))
(public-define (pair-term x)
  (fresh (y z) (pair-term-of x y z) (term y) (term z)))

(public-define nil-term `nil-term)


;is x term?
(public-define (term x)
  (conde
   [(value x)]
   [(variable x)]
   [(pair-term x)]
   [(== x nil-term)]
   [(hash-expr x)]
   ))


(public-define (term-list x)
  (conde
   [(== x `())]
   [(fresh (head tail) (== `(,head . ,tail) x) (term head) (term-list tail))]
   ))


;FREE NAME

;N is in fn(M)
(public-define (free-name-term M N)
               (conde
                [(free-name-value M N) (value M)]
                [(fresh (n m) (pair-term-of M n m) (conde
                                              [(free-name-term n N)] [(free-name-term m N)]))]
                [(fresh (n) (hash-expr-of M n) (free-name-term n N))]))

;N is not in fn(M)
(public-define (not-free-name-term M N)
               (conde
                [(not-free-name-value M N) (value M)]
                [(variable M)]
                [(fresh (n m) (pair-term-of M n m) (not-free-name-term n N) (not-free-name-term m N))]
                [(fresh (n) (hash-expr-of M n) (not-free-name-term n N))]))

;FREE VARIABLE
;is v in fv(U)?

(public-define (free-variable-term T v)
               (conde
                [(value T) (free-variable-value T v)]
                [(variable T) (== T v)]
                [(fresh (x y) (pair-term-of T x y)
                        (conde
                         [(free-variable-term x v)]
                         [(free-variable-term y v)]))]
                [(fresh (x) (hash-expr-of T x) (free-variable-term x v))]))


(public-define (not-free-variable-term T v)
               (conde
                [(== nil-term T)]
                [(value T) (not-free-variable-value T v)]
                [(variable T) (=/= T v)]
                [(fresh (x y) (pair-term-of T x y)
                        (not-free-variable-term x v)(not-free-variable-term y v))]
                [(fresh (x) (hash-expr-of T x) (not-free-variable-term x v))]))

(define (not-free-variable-term-list L v)
  (conde
   [(== L `())]
   [(fresh (head body) (== L `(,head . ,body))
           (not-free-variable-term head v) (not-free-variable-term-list body v))]))


;#SET OF FREE VALUE

;is fv(U) subset of V?

;is fv(T) subset of V?
(public-define (super-free-variable-term  T V)
  (conde
   [(value T) (super-free-variable-value T V)]
   [(variable T) (contains V T)]
   [(fresh (x y) (pair-term-of T x y) (super-free-variable-term x V) (super-free-variable-term y V))]
   [(fresh (x) (hash-expr-of T x) (super-free-variable-term x V))]
   ))

;is fv(T) subset of V for all term T in L?
(public-define (super-free-value-term-list L V)
  (conde
   [(== L `())]
   [(fresh (head body) (== L `(,head . ,body)) (super-free-variable-term head V) (super-free-value-term-list body V))]
   ))





