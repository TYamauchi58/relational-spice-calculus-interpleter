#lang racket
(require "../faster-minikanren/mk.rkt")
(require "public-define.rkt")
(require "value.rkt")
(require "term.rkt")
(require "cost.rkt")

(public-define (eval-term T V C)
  (conde
   [(eval-pair T V C)]
   [(eval-nil T V C)]
   [(eval-value T V C)]
   [(eval-hash T V C)]))



(define (eval-pair T V C)
  (fresh (t1 t2 v1 v2 c1 c2 c3)
         (pair-term-of T t1 t2)
         (eval-term t1 v1 c1)
         (eval-term t2 v2 c2)
         (pair-value-of V v1 v2)
         (cost-add c3 c1 c2)
         (cost-add C c3 cost-pair)))

(define (eval-nil T V C)
  (conde
   [(== T nil-term) (== V nil-value) (== C cost-zero)]))


(define (eval-value T V C)
  (conde
   [(== T V) (value T) (== C cost-zero)]))

(define (eval-hash T V C)
  (fresh (t v c)
         (hash-expr-of T t)
         (hash-value-of V v)
         (cost-add C cost-hash c)
         (eval-term t v c)))

