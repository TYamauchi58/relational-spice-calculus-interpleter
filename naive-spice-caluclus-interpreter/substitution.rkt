#lang racket
(require "../faster-minikanren/mk.rkt")
(require "public-define.rkt")
(require "value.rkt")
(require "term.rkt")
(require "process.rkt")



(public-define (substitute-value Q P L x)
               (conde
                [(constant P) (== P Q)]
                [(== P nil-value) (== Q nil-value)]
                [(name P) (== P Q)]
                [(variable P) (=/= P x) (== P Q)]
                [(== P x) (== Q L)]
                [(fresh (p1 p2 q1 q2)
                        (pair-value-of P p1 p2)
                        (pair-value-of Q q1 q2)
                        (substitute-value q1 p1 L x)
                        (substitute-value q2 p2 L x))]
                [(fresh (p q)
                        (hash-value-of P p)
                        (hash-value-of Q q)
                        (substitute-value q p L x))]))

(public-define (substitute-term Q P L x)
               (conde
                [(== P nil-value)(== Q nil-value)]
                [(substitute-value Q P L x)]
                [(fresh (p1 p2 q1 q2)
                        (pair-term-of P p1 p2)
                        (pair-term-of Q q1 q2)
                        (substitute-term q1 p1 L x)
                        (substitute-term q2 p2 L x))]
                [(fresh (p q)
                        (hash-expr-of P p)
                        (hash-expr-of Q q)
                        (substitute-term q p L x))]))


;Q=P[L/x]
(public-define (substitute Q P L x)
  (conde
   [(substitute-stop Q P L x)]
   [(substitute-out Q P L x)]
   [(substitute-inp-ref Q P L x)]
   [(substitute-inp-skip Q P L x)]
   [(substitute-parallel Q P L x)]
   [(substitute-new Q P L x)]
   [(substitute-repeat Q P L x)]
   [(substitute-match Q P L x)]
   [(substitute-split Q P L x)]
   [(substitute-store Q P L x)]
   [(substitute-free1 Q P L x)]
   [(substitute-free2 Q P L x)]))

(public-define (substitute-out Q P L x)
               (fresh (m n r M N R)
                      (output--of P m n r)
                      (output--of Q M N R)
                      (substitute-term M m L x)
                      (substitute-term N n L x)
                      (substitute R r L x)))

(public-define (substitute-inp-ref Q P L x)
               (fresh (m p M)
                      (input--of P m x p)
                      (input--of Q M x p)
                      (substitute-term M m L x)))


(public-define (substitute-inp-skip Q P L x)
               (fresh (m y p M q)
                      (=/= x y)
                      (input--of P m y p)
                      (input--of Q M y q)
                      (substitute-term M m L x)
                      (substitute q p L x)))

(public-define (substitute-parallel Q P L x)
               (fresh (q1 q2 p1 p2)
                      (parallel-of Q q1 q2)
                      (parallel-of P p1 p2)
                      (substitute q1 p1 L x)
                      (substitute q2 p2 L x)))

(public-define (substitute-new Q P L x)
               (fresh (n p q)
                      (new-of Q n q)
                      (new-of P n p)
                      (substitute q p L x)))

(public-define (substitute-repeat Q P L x)
               (fresh (p q)
                      (repeat-of P p)
                      (repeat-of Q q)
                      (substitute q p L x)))

(public-define (substitute-stop Q P L x)
                 (conde
                  [(== Q `())(== P `())]))

(public-define (substitute-match Q P L x)
               (fresh (m n p q m* n* p* q*)
                      (match-of P m n p q)
                      (match-of Q m* n* p* q*)
                      (substitute-term m* m L x)
                      (substitute-term n* n L x)
                      (substitute p* p L x)
                      (substitute q* q L x)))

(define (substitute-split Q P L x)
  (fresh (u v m r p m* r* p*)
         (split-of P u v m r p)
         (split-of Q u v m* r* p*)
         (substitute-term m* m L x)
         (conde
          [(== x u) (== p p*)]
          [(== x v) (== p p*)]
          [(=/= x u) (=/= x v) (substitute p* p L x)])
         (substitute r* r L x)))


(define (substitute-store Q P L x)
  (fresh (y m p m* p*)
         (store-of P y m p)
         (store-of Q y m* p*)
         (=/= y x)
         (substitute-term m* m L x)
         (substitute p* p L x)))

(define (substitute-free1 Q P L x)
  (fresh (p)
         (free-of P x p)
         (== Q P)))

(define (substitute-free2 Q P L x)
  (fresh (p p* x*)
         (=/= x x*)
         (free-of P x* p)
         (free-of Q x* p*)
         (substitute p* p L x)))


