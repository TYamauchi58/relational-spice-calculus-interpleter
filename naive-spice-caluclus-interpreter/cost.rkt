#lang racket
(require "../faster-minikanren/mk.rkt")
(require "public-define.rkt")
(require "value.rkt")
(require "term.rkt")
(require "process.rkt")
(require "typing.rkt")


;###DEFINITION OF COST VALUE
;is x cost-base
(public-define cost-zero `())
(public-define cost-pair `((+ pair)))
(public-define cost-hash `((+ hash)))
(public-define cost-match `((+match)))
(public-define cost-repeat `((+ repeat)))

(public-define (cost-store-of c x)
               (== c `((+ (store ,x)))))

(define (cost-base x)
  (fresh (v)
         (cost-figure v)
         (conde
          [(== x `(+ ,v))]
          [(== x `(- ,v))])))

(define (cost-figure x)
  (conde
   [(fresh (v) (variable v) (== x `(store ,v)))]
   [(== x `pair)]
   [(== x `hash)]
   [(== x `match)]
   [(== x `repeat)]
   ))

(public-define (cost x)
               (conde
                [(== x cost-zero)]
                [(fresh (xh xb)
                        (== x `(,xh . ,xb))
                        (cost-base xh)
                        (cost xb))]))

;x=y+z
(define (cost-add-pos x y z)
               (conde
                [(== y `()) (== x z)]
                [(fresh (xh xb yb)
                        (== x `(,xh . ,xb))
                        (== y `(,xh . ,yb))
                        (cost-add-pos xb yb z))]))

(public-define (cost-add x y z)
               (conde
               [(== y cost-zero) (== x z)]
               [(fresh (xh xb yb)
                       (== x `(,xh . ,xb))
                       (== y `(,xh . ,yb))
                       (cost-add xb yb z))]))

(define (cost-base-neg x y)
  (conde
   [(fresh (v) (== x `(+ ,v)) (== y `(- ,v)))]
   [(fresh (v) (== x `(- ,v)) (== y `(+ ,v)))]))

(public-define (cost-neg x y)
               (conde
                [(== x cost-zero) (== y cost-zero)]
                [(fresh (xh xb yh yb)
                        (== x `(,xh . ,xb))
                        (== y `(,yh . ,yb ))
                        (cost-base-neg xh yh)
                        (cost-neg xb yb))]
                ))



;COST ass

(public-define empty-cost-ass `())

(public-define (cost-ass-node-of n a c)
  (== n `(,a ,c)))

(define (cost-ass n)
  (conde
   [(== n empty-cost-ass)]
   [(fresh (nh nb)
           (== n `(,nh . ,nb))
           (cost-ass-node nh)
           (cost-ass nb))]))

(public-define (cost-ass-node a)
               (fresh (c s)
                      (cost-ass-node-of a c s)
                      (cost s)))

(public-define (cost-ass-of ass a c)
               (conde
                [(== c cost-zero) (== ass empty-cost-ass)]
                [(fresh (assh assb ch cb)
                        (== ass `(,assh . ,assb))
                        (== c `(,ch . ,cb))
                        (cost-ass-node-of assh a ch)
                        (cost-ass-of assb a cb))]))

;x=y+z
(public-define (cost-ass-add x y z)
               (conde
               [(== y empty-cost-ass) (== x z)]
               [(fresh (xh xb yb)
                       (== x `(,xh . ,xb))
                       (== y `(,xh . ,yb))
                       (cost-ass-add xb yb z))]))
