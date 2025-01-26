#lang racket
(require "../faster-minikanren/mk.rkt")
(require "public-define.rkt")
(require "value.rkt")
(require "term.rkt")
(require "process.rkt")
(require "typing.rkt")
(require "cost.rkt")
(require "eval.rkt")
(require "substitution.rkt")

;P > Q
(public-define (red-process P Q C)
  (conde
   [(red-repeat P Q C)]
   [(red-store P Q C)]
   [(red-free P Q C)]
   [(red-match P Q C)]
   [(red-match-err P Q C)]
   [(red-split P Q C)]
   [(red-split-err P Q C)]))

(define (red-repeat P Q C)
  (fresh (p)
         (repeat-of P p)
         (parallel-of Q p P)
         (== C cost-repeat)))

(define (red-store P Q C)
  (fresh (x m p v pvx c cs)
         (store-of P x m p)
         (eval-term m v c)
         (substitute Q p v x)
         (cost-store-of cs x)
         (cost-add C c cs)))

(define (red-free P Q C)
  (fresh (x c)
         (free-of P x Q)
         (cost-store-of c x)
         (cost-neg C c)))

(define (red-match P Q C)
  (fresh (m n p u v c d cd)
         (match-of P m n p Q)
         (eval-term m u c)
         (eval-term n v d)
         (value-eq u v)
         (cost-add cd c d)
         (cost-add C cost-match cd)))

(define (red-match-err P Q C)
  (fresh (m n q v w c d cd)
         (match-of P m n Q q)
         (eval-term m v c)
         (eval-term n w d)
         (value-neq v w)
         (cost-add cd c d)
         (cost-add C cost-match cd)))

(define (red-split P Q C)
  (fresh (u w m r p v vh vb c0 c1 o cu cv)
         (split-of P u w m r p)
         (eval-term m v c0)
         (pair-value-of v vh vb)
         (substitute o p vh u)
         (substitute Q o vb w)
         (cost-store-of cu u)
         (cost-store-of cv w)
         (cost-add c1 c0 cu)
         (cost-add C c1 cv)))



(define (red-split-err P Q C)
  (fresh (u w m p v)
         (split-of P u w m Q p)
         (eval-term m v C)
         (conde
          [(== v nil-value)]
          [(fresh (v1 v2) (== v `(,v1 . ,v2)) (=/= v1 `pair-value))])))

;A|-P>>Q:S
(public-define (mred-process A P Q S)
               (conde
                [(mred-refl A P Q S)]
                [(mred-single A P Q S)]
                [(mred-trans A P Q S)]
                [(mred-par A P Q S)]
                [(mred-restr A P Q S)]))

(define (mred-refl A P Q S)
  (conde
   [(== P Q) (== S empty-cost-ass)]))

(define (mred-single A P Q S)
  (fresh (a v c)
         (calc-type-of A a v)
         (cost-ass-of S a c)
         (red-process P Q c)
         ))

(define (mred-trans A P Q S)
  (fresh (B R s1 s2)
         (mred-process A P R s1)
         (type-eq A B)
         (mred-process B R Q s2)
         (cost-ass-add S s1 s2)))

(define (mred-par A P Q S)
  (fresh (a b p1 p2 q1 q2 s t)
         (type-represent-composition A a b)
         (parallel-of P p1 p2)
         (parallel-of Q q1 q2)
         (mred-process a p1 q1 s)
         (mred-process b p2 q2 t)
         (cost-ass-add S s t)
  ))

(define (mred-restr A P Q S)
  (fresh (p q n)
         (new-of P n p)
         (new-of Q n q)
         (mred-process A p q S)))



