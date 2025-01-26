#lang racket
(require "../faster-minikanren/mk.rkt")
(require "mklist.rkt")
(require "value.rkt")
(require "term.rkt")
(require "process.rkt")
(require "agent.rkt")
(require "public-define.rkt")


;#DEFINITION OF TYPE

;is a calc-name
(public-define (calc-name-of a n)
  (== a `(calc n)))
(public-define (calc-name a)
  (fresh (n) (calc-name-of a n)))

;is t calc-type?
(public-define (calc-type-of t a l)
  (== t `(calc-type ,a ,l)))
(public-define (calc-type t)
  (fresh (a l) (calc-type-of t a l) (calc-name a) (variable-list l)))

;is t composite type?
(public-define (composite-type-of t p q)
  (== t `(composite-type ,p ,q)))
(public-define (composite-type t)
  (fresh (p q) (composite-type-of t p q) (type p) (type q)))

;is t type?
(public-define (type t)
  (conde
   [(calc-type t)]
   [(composite-type t)]
   ))

;###TYPING OF AGENT
(public-define (type-agent T A)
  (conde
   [(typeout T A)]
   [(typein T A)]
   [(typecomp T A)]
   [(typerestr T A)]
   [(typerepl T A)]
   [(typestore T A)]
   [(typenil T A)]
   [(typefree T A)]
   [(typematch T A)]
   [(typesplit T A)]
   [(typeabstr T A)]
   [(typeconcr T A)]
   ))

(define (typeout T A)
  (fresh (a v n m p)
         (calc-type-of T a v)
         (output--of A n m p)
         (type-agent T p)
         (super-free-variable-term n v)
         (super-free-variable-term m v)))


(define (typein T A)
  (fresh (a v m x xv p Tx)
         (input--of A m x p)
         (calc-type-of T a v)
         (append xv x v)
         (calc-type-of Tx a xv)
         (type-agent Tx p)
         (uncontains v x)
         (super-free-variable-term m v)
         ))

(define (typecomp T A)
  (fresh (a b p q)
         (parallel-of A a b)
         (composite-type-of T p q)
         (type-agent p a)
         (type-agent q b)))

(define (typerestr T A)
  (fresh (n p)
         (new-of A n p)
         (type-agent T p)))

(define (typerepl T A)
  (fresh (a p)
         (calc-type-of T a `())
         (repeat-of A p)
         (type-agent T p)))

(define (typenil T A)
  (fresh (a)
         (calc-type-of T a `())
         (== A `())))

(define (typestore T A)
  (fresh (a V x xV xT m p)
         (calc-type-of T a V)
         (store-of A x m p)
         (append xV x V)
         (calc-type-of xT a xV)
         (type-agent xT p)
         (uncontains V x)
         (super-free-variable-term m V)
         ))

(define (typefree T A)
  (fresh (a x v xv p t)
         (free-of A x p)
         (calc-type-of T a xv)
         (calc-type-of t a v)
         (remove v x xv)
         (type-agent t p)
         (contains xv x)
         ))

(define (typematch T A)
  (fresh (a v m n p q)
         (calc-type-of T a v)
         (match-of A m n p q)
         (super-free-variable-term m v)
         (super-free-variable-term n v)))

(define (typesplit T A)
  (fresh (a v x y xyv yv m r p t)
         (calc-type-of T a v)
         (append yv y v)
         (append xyv x yv)
         (calc-type-of t a xyv)
         (split-of A x y m r p)
         (type-agent t p)
         (type-agent T r)
         (uncontains v x)
         (uncontains v y)
         (super-free-variable-term m v)
         ))

(define (typeabstr T A)
  (fresh (x p)
         (abstract-of A x p)
         (type-agent T p)))

(define (typeconcr T A)
  (fresh (l m p)
         (concrete-of A l m p)
         (type-agent T p)
         (super-free-variable-term `() m)))


;#TYPE EQUIVALENT

(public-define (calc-type-class-of R a)
  (== R `(calc-type-class ,a)))
(public-define (calc-type-class R)
  (fresh (a) (calc-type-class-of R a)))

(public-define (composite-type-class-of R r s)
  (== R `(composite-type-class ,r ,s)))
(public-define (composite-type-class R)
  (fresh (r s) (composite-type-class-of R r s)))


(public-define (type-class R)
  (conde
   [(calc-type-class R)]
   [(composite-type-class R)]))

(define (calc-type-represent R T)
  (conde
   [(fresh (r v) (calc-type-of T r v) (calc-type-class-of R r))]
   [(fresh (A B) (composite-type-of T A B) (calc-type-represent R A) (calc-type-represent R B))]))

(define (composite-type-represent R T)
  (conde
   [(fresh (a b A B) (composite-type-class-of R a b) (composite-type-of T A B) (calc-type-represent a A) (calc-type-represent b B) (=/= a b))]
   [(fresh (a b A B) (composite-type-class-of R a b) (composite-type-of T A B) (composite-type-represent a A) (calc-type-represent b B))]
   [(fresh (a b A B) (composite-type-class-of R a b) (composite-type-of T A B) (calc-type-represent a A) (composite-type-represent b B))]
   [(fresh (a b A B) (composite-type-class-of R a b) (composite-type-of T A B) (composite-type-represent a A) (composite-type-represent b B))]))

(public-define (type-represent R T)
  (conde
   [(calc-type-represent R T)]
   [(composite-type-represent R T)]))


(public-define (type-represent-composition R P Q)
               (conde
                [(== R P) (== P Q) (fresh (c) (calc-type-class-of P c))]
                [(composite-type-class-of R P Q)
                 (conde
                  [(fresh (c1 c2) (calc-type-class-of P c1) (calc-type-class-of Q c2) (=/= c1 c2))]
                  [(fresh (c) (calc-type-class-of P c)) (fresh (q1 q2) (composite-type-class-of Q q1 q2))]
                  [(fresh (p1 p2) (composite-type-class-of P p1 p2)) (fresh (c) (calc-type-class-of Q c))]
                  [(fresh (p1 p2) (composite-type-class-of P p1 p2)) (fresh (q1 q2) (composite-type-class-of Q q1 q2))]
                  )]
                ))

(public-define (type-eq T U)
               (fresh (r) (type-represent r T) (type-represent r U)))
