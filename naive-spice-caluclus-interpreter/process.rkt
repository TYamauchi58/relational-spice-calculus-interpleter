#lang racket
(require "../faster-minikanren/mk.rkt")
(require "public-define.rkt")
(require "mklist.rkt")
(require "value.rkt")
(require "term.rkt")

;#DEFINITION OF PROCESS

;is p output?
(public-define (output--of p m n q)
  (== p `((out ,m ,n). ,q)))
(public-define (output p)
  (fresh (m n q) (output--of p m n q) (term m) (term n) (process q)))

;is p input?
(public-define (input--of p m x q)
  (== p `((inp ,m ,x). ,q)))
(public-define (input p)
  (fresh (m x q) (input--of p m x q) (term m) (variable x) (process q)))

;is p pararell?
(public-define (parallel-of p q r)
  (== p `(parallel ,q ,r)))
(public-define (parallel p)
  (fresh (q r) (parallel-of p q r) (process q) (process r)))

;is p new?
(public-define (new-of p n q)
  (== p `((new ,n ). ,q)))
(public-define (new p)
  (fresh (n q) (new-of p n q) (name n) (process q)))

;is p repeat?
(public-define (repeat-of p q)
  (== p `(repeat ,q)))
(public-define (repeat p)
  (fresh (q) (repeat-of p q) (process q)))

;is p store?
(public-define (store-of p x m q)
  (== p `((store ,x ,m). ,q)))
(public-define (store p)
  (fresh (x m q) (store-of p x m q) (variable x) (term m) (process q)))

;is p free?
(public-define (free-of p x q)
  (== p `((free ,x). ,q)))
(public-define (free p)
  (fresh (q x) (free-of p x q) (variable x) (process q)))

;is p match?
(public-define (match-of p m n q r)
  (== p `((match ,m ,n ,q). ,r)))
(public-define (match p)
  (fresh (m n q r) (match-of p m n q r) (term m) (term n) (process q) (process r)))

;is p split?
(public-define (split-of p u v m q r)
  (== p `((split (,u ,v) ,m ,q). ,r)))
(public-define (split p)
  (fresh (u v m q r) (split-of p u v m q r) (variable u) (variable v) (term m) (process q) (process r)))

;is p process?

(public-define (process p)
  (conde
   [(== p `())];stop
   [(output p)]
   [(input p)]
   [(parallel p)]
   [(new p)]
   [(repeat p)]
   [(store p)]
   [(match p)]
   [(split p)]
   [(free p)]
   ))



;FREE NAME

;N is in fn(P)
(public-define (free-name-process P N)
               (conde
                [(fresh (m n p) (input--of P m n p) (conde
                                                    [(free-name-term m N)]
                                                    [(free-name-term n N)]
                                                    [(free-name-process p N)]))]
                [(fresh (m x p) (output--of P m x p) (conde
                                                     [(free-name-term m N)]
                                                     [(free-name-process p N)]))]
                [(fresh (p q) (parallel-of P p q) (conde
                                                   [(free-name-process p N)]
                                                   [(free-name-process q N)]))]
                [(fresh (n p) (new-of P n p) (=/= n N) (free-name-process p N))]
                [(fresh (p) (repeat-of P p) (free-name-process p N))]
                [(fresh (x m p) (store-of P x m p) (free-name-process p N))]
                [(fresh (x p) (free-of P x p) (free-name-process p N))]
                [(fresh (m n p q) (match-of P m n p q) (conde
                                                        [(free-name-term m N)]
                                                        [(free-name-term n N)]
                                                        [(free-name-process p N)]
                                                        [(free-name-process q N)]))]
                [(fresh (u v m r p) (split-of P u v m r p) (conde
                                                        [(free-name-term m N)]
                                                        [(free-name-process p N)]
                                                        [(free-name-process r N)]))]))

;N is not in fn(P)
(public-define (not-free-name-process P N)
               (conde
                [(== P `())]
                [(fresh (m n p) (input--of P m n p) (not-free-name-term m N) (not-free-name-term n N) (not-free-name-process p N))]
                [(fresh (m x p) (output--of P m x p) (not-free-name-term m N) (not-free-name-process p N))]
                [(fresh (p q) (parallel-of P p q) (not-free-name-process p N) (not-free-name-process q N))]
                [(fresh (n p) (new-of P n p) (conde
                                              [(=/= n N)]
                                              [(not-free-name-process p N)]))]
                [(fresh (p) (repeat-of P p) (not-free-name-process p N))]
                [(fresh (x m p) (store-of P x m p) (not-free-name-process p N))]
                [(fresh (x p) (free-of P x p) (not-free-name-process p N))]
                [(fresh (m n p q) (match-of P m n p q) (not-free-name-term m N) (not-free-name-term n N)
                        (not-free-name-process p N) (not-free-name-process q N))]
                [(fresh (u v m r p) (split-of P u v m r p) (not-free-name-term m N) (not-free-name-process p N) (not-free-name-process r N))]))



;FREE VARIABLE
;N is in fn(P)
(public-define (free-variable-process P v)
               (conde
                [(fresh (m n p) (input--of P m n p) (conde
                                                    [(free-variable-term m v)]
                                                    [(free-variable-term n v)]
                                                    [(free-name-process p v)]))]
                [(fresh (m x p) (output--of P m x p) (conde
                                                     [(free-variable-term m v)]
                                                     [(free-variable-process p v)]))]
                [(fresh (p q) (parallel-of P p q) (conde
                                                   [(free-variable-process p v)]
                                                   [(free-variable-process q v)]))]
                [(fresh (n p) (new-of P n p) (free-variable-process p v))]
                [(fresh (p) (repeat-of P p) (free-variable-process p v))]
                [(fresh (x m p) (store-of P x m p) (=/= x v) (free-variable-process p v))]
                [(fresh (x p) (free-of P x p) (free-variable-process p v))]
                [(fresh (m n p q) (match-of P m n p q) (conde
                                                        [(free-variable-term m v)]
                                                        [(free-variable-term n v)]
                                                        [(free-variable-process p v)]
                                                        [(free-variable-process q v)]))]
                [(fresh (x y m r p) (split-of P x y m r p) (conde
                                                        [(free-variable-term m v)]
                                                        [(free-variable-process p v) (=/= v x) (=/= v y)]
                                                        [(free-variable-process r v)]))]))
;is v in fv(U)?

(public-define (not-free-variable-process P v)
               (conde
                [(fresh (m n p) (output--of P m n p) (not-free-variable-term m v) (not-free-variable-term n v) (not-free-variable-process p v))]
                [(fresh (m x p) (input--of P m x p)
                        (conde
                         [(== x v)]
                         [(not-free-variable-term m v) (not-free-variable-process p v)]))]
                [(fresh (p q) (parallel-of P p q) (not-free-variable-process p v) (not-free-variable-process q v))]
                [(fresh (n p) (new-of P n p) (not-free-variable-process p v))]
                [(fresh (p) (repeat-of P p) (not-free-variable-process p v))]
                [(== P `())]
                [(fresh (x m p) (store-of P x m p)
                        (conde
                         [(== v x)]
                         [(not-free-variable-process p v)]))]
                [(fresh (x p) (free-of P x p) (not-free-variable-process p v))]
                [(fresh (m n p q) (match-of P m n p q)
                        (not-free-variable-term m v)
                        (not-free-variable-term n v)
                        (not-free-variable-process p v)
                        (not-free-variable-process q v))]
                [(fresh (u w m r p) (split-of P u w m r p)
                        (not-free-variable-term m v)
                        (not-free-variable-process r v)
                        (conde
                         [(not-free-variable-process p v)]
                         [(== u v)]
                         [(== w v)]))]))

;#SET OF FREE VALUE
;is fv(P) subset of V?

(public-define (super-free-variable-process P V)
  (conde
   [(fresh (m n q) (output--of P m n q) (super-free-variable-term m V) (super-free-variable-term n V) (super-free-variable-process q V))]
   [(fresh (m x q) (input--of P m x q) (super-free-variable-term m `(,x . V)) (super-free-variable-process q `(,x . ,V)))]
   [(fresh (q r) (parallel-of P q r) (super-free-variable-process q V) (super-free-variable-process r V))]
   [(fresh (x q) (new-of P x q) (super-free-variable-process q `(,x . ,V)))]
   [(fresh (q) (repeat-of P q) (super-free-variable-process q V))]
   [(== P `())];stop
   [(fresh (x m q) (store-of P x m q) (super-free-variable-process q `(,x . ,V)))]
   [(fresh (m n q r) (match-of P m n q r) (super-free-variable-term m V) (super-free-variable-term n V) (super-free-variable-process q V) (super-free-variable-process r V))]
   [(fresh (u v m r q) (split-of P u v m r q)
           (fresh (uv-V) (== uv-V `(,u ,v . ,V)) (super-free-variable-process q uv-V))
           (super-free-variable-term m V)
           (super-free-variable-process r V))]
   ))




