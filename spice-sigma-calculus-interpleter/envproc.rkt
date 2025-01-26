#lang racket
(require "../faster-minikanren/mk.rkt")
(require "public-define.rkt")
(require "mklist.rkt")
(require "value.rkt")
(require "term.rkt")
(require "process.rkt")
(require "environment.rkt")

(public-define (env-app-of EP P E)
               (== EP `(env-app ,P ,E)))
(public-define (env-app EP)
               (fresh (P E) (env-app-of EP P E) (process P) (environment E)))

(public-define (parallel-envproc-of P Q R)
               (== P `(parallel ,Q ,R)))
(public-define (parallel-envproc P)
               (fresh (Q R) (parallel-envproc-of P Q R) (envproc Q) (envproc R)))


;is p new?
(public-define (new-envproc-of p n q)
  (== p `((new ,n ) . ,q)))
(public-define (new-envproc p)
  (fresh (n q) (new-envproc-of p n q) (name n) (envproc q)))


(public-define (envproc P)
               (conde
                [(env-app P)]
                [(parallel-envproc P)]
                [(new-envproc P)]))


(public-define (subst-envproc Q P V x)
               (conde
                [(fresh (p e f) (env-app-of P p e) (env-app-of Q p f) (env-extn-of f V x e))]
                [(fresh (p1 p2 q1 q2) (parallel-envproc-of P p1 p2) (parallel-envproc-of Q q1 q2)
                        (subst-envproc q1 p1 V x) (subst-envproc q2 p2 V x))]
                [(fresh (p n q) (new-envproc-of P n p) (new-envproc-of Q n q) (subst-envproc q p V x))]))


;FREE NAME
(public-define (not-free-name-envproc EP N)
               (conde
                [(fresh (P E) (env-app-of EP P E) (not-free-name-process P N) (not-free-name-environment E N))]
                [(fresh (P Q) (parallel-envproc-of EP P Q) (not-free-name-envproc P N) (not-free-name-envproc Q N))]
                [(fresh (n P) (new-envproc-of EP n P)
                        (conde
                         [(== N n)]
                         [(not-free-name-envproc P N)]))]))
