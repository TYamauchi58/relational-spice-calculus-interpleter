#lang racket
(require "../faster-minikanren/mk.rkt")
(require "public-define.rkt")
(require "value.rkt")
(require "term.rkt")
(require "cost.rkt")
(require "environment.rkt")

(public-define (eval-term E T V C)
  (conde
   [(eval-pair E T V C)]
   [(eval-nil E T V C)]
   [(eval-value E T V C)]
   [(eval-lookup E T V C)]
   [(eval-hash E T V C)]))



(define (eval-pair E T V C)
  (fresh (t1 t2 v1 v2 c1 c2 c3)
         (pair-term-of T t1 t2)
         (eval-term E t1 v1 c1)
         (eval-term E t2 v2 c2)
         (pair-value-of V v1 v2)
         (cost-add c3 c1 c2)
         (cost-add C c3 cost-pair)))

(define (eval-nil E T V C)
  (conde
   [(== T nil-term) (== V nil-value) (== C cost-zero)]))


(define (eval-value E T V C)
  (conde
   [(== T V) (value T) (== C cost-zero)]))

(define (eval-lookup E T V C)
  (fresh (Eb) (variable T) (lookup V T E) (== C cost-zero)))

(define (eval-hash E T V C)
  (fresh (t v c)
         (hash-expr-of T t)
         (hash-value-of V v)
         (cost-add C cost-hash c)
         (eval-term E t v c)))

