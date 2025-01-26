#lang racket
(require "../faster-minikanren/mk.rkt")
(require "public-define.rkt")

(public-define (contains L x)
               (conde
                [(fresh (l) (== L `(,x . ,l)))]
                [(fresh (l k) (== L `(,k . ,l)) (contains l x))]))

(public-define (uncontains L x)
               (conde
                [(== L `())]
                [(fresh (l k) (== L `(,k . ,l)) (=/= k x) (uncontains l x))]))

(public-define (append K x L)
               (== K `(,x . ,L)))

(public-define (remove K x L)
               (conde
                [(== L `()) (== K `())]
                [(fresh (l2 k2) (== L `(,x . ,l2))
                        (remove K x l2))]
                [(fresh (l1 l2 k2) (== L `(,l1 . ,l2)) (== K `(,l1 . ,k2))
                        (=/= l1 x) (remove k2 x l2))]))