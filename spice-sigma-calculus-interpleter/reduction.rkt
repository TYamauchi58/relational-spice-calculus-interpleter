#lang racket
(require "../faster-minikanren/mk.rkt")
(require "public-define.rkt")
(require "value.rkt")
(require "term.rkt")
(require "process.rkt")
(require "environment.rkt")
(require "envproc.rkt")
(require "typing.rkt")
(require "cost.rkt")
(require "eval.rkt")

;P > Q
(public-define (red-process P Q C)
  (conde
   [(red-repeat P Q C)]
   [(red-store P Q C)]
   [(red-free P Q C)]
   [(red-match P Q C)]
   [(red-match-err P Q C)]
   [(red-split P Q C)]
   [(red-split-err P Q C)]
   [(red-parallel P Q C)]
   [(red-new P Q C)]))

(define (red-repeat P Q C)
  (fresh (e p q r)
         (env-app-of P p e)
         (repeat-of p r)
         (parallel-of q r P)
         (env-app-of Q q e)
         (== C cost-repeat)))

(define (red-store P Q C)
  (fresh (p x m q v e f)
         (env-app-of P p e)
         (store-of p x m q)
         (env-extn-of f v x e)
         (env-app-of Q q f)
         (eval-term e m v C)))

(define (red-free P Q C)
  (fresh (p q e f x)
         (env-app-of P p e)
         (env-app-of Q q f)
         (free-of p x q)
         (remove-env f x e)
         (== C cost-zero)))

(define (red-match P Q C)
  (fresh (p m n q r u v cd c d e)
         (env-app-of P p e)
         (env-app-of Q q e)
         (match-of p m n r q)
         (eval-term e m u c)
         (eval-term e n v d)
         (value-eq u v)
         (cost-add cd c d)
         (cost-add C cost-match cd)))

(define (red-match-err P Q C)
  (fresh (p m n q r u v cd c d e)
         (env-app-of P p e)
         (env-app-of Q q e)
         (match-of p m n q r)
         (eval-term e m u c)
         (eval-term e n v d)
         (value-neq u v)
         (cost-add cd c d)
         (cost-add C cost-match cd)))

(define (red-split P Q C)
  (fresh (e p f q u w m r v k l g)
         (env-app-of P p e)
         (env-app-of Q q f)
         (split-of p u w m r q)
         (eval-term e m v C)
         (pair-value-of v k l)
         (env-extn-of g k u e)
         (env-extn-of f l w g)))

(define (red-split-err P Q C)
  (fresh (e p q u w m r v k l g)
         (env-app-of P p e)
         (env-app-of Q q e)
         (split-of p u w m q r)
         (eval-term e m v C)
         (conde
          [(== v nil-value)
           (fresh (a b) (== v `(,a . ,b)) (=/= a `pair-value))])))


(define (red-parallel P Q C)
  (fresh (p p1 p2 q1 q2 E)
         (env-app-of P p E)
         (parallel-of p p1 p2)
         (env-app-of q1 p1 E)
         (env-app-of q2 p2 E)
         (parallel-envproc-of Q q1 q2)
         (== C cost-zero)))

(define (red-new P Q C)
  (fresh (p p1 q E n)
         (env-app-of P p E)
         (new-of p n p1)
         (env-app-of q p1 E)
         (new-envproc-of Q n q)
         (== C cost-zero)))



;A|-P>>Q:S
(public-define (mred-process A P Q S)
               (fresh (r)
                      (mred-process@ r P Q S)
                      (type-represent r A)))

(public-define (mred-process@ A P Q S)
               (conde
                [(== P Q) (== S empty-cost-ass)]
                [(mred+-process A P Q S)]))

(define (mred+-process A P Q S)
               (conde
                [(mred-append A P Q S)]
                [(mred-par A P Q S)]
                [(mred-restr A P Q S)]))

(define (mred-refl A P Q S)
  (conde
   [(== P Q) (== S empty-cost-ass)]))

(define (mred-append A P Q S)
  (fresh (a c1 s1 s2 p)
         (calc-type-class-of A a)
         (red-process P p c1)
         (mred-process@ A p Q s1)
         (cost-ass-of s2 a c1)
         (cost-ass-add S s1 s2)))

(define (mred-par A P Q S)
  (conde
  [(fresh (a b p1 p2 q1 q2 s t)
         (type-represent-composition A a b)
         (parallel-envproc-of P p1 p2)
         (parallel-envproc-of Q q1 q2)
         (conde
          [(mred+-process a p1 q1 s) (mred+-process b p2 q2 t) (cost-ass-add S s t)]
          [(mred+-process a p1 q1 S) (== p2 q2)]
          [(== p1 q1) (mred+-process b p2 q2 S)])
         )]
  ))

(define (mred-restr A P Q S)
  (fresh (p q n)
         (new-envproc-of P n p)
         (new-envproc-of Q n q)
         (mred+-process A p q S)))




