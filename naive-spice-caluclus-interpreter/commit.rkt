#lang racket
(require "../faster-minikanren/mk.rkt")
(require "public-define.rkt")
(require "value.rkt")
(require "term.rkt")
(require "process.rkt")
(require "agent.rkt")
(require "typing.rkt")
(require "cost.rkt")
(require "eval.rkt")
(require "substitution.rkt")
(require "reduction.rkt")

(public-define action-none `action-none)

(public-define (action-in-of a n)
               (== a `(action-in . ,n)))
(public-define (action-out-of a n)
               (== a `(action-out . ,n)))

;P=A@B
(define (inter P A B)
               (conde
                [(inter1 P A B)]
                [(inter2 P A B)]))

(define (inter1 P A B)
  (fresh (x a l m b ax ab)
         (abstract-of A x a)
         (concrete-of B l m b)
         (substitute ax a m x)
         (parallel-of ab ax b)
         (new-all P l ab)))

(define (inter2 P A B)
  (fresh (x a l m b bx ab)
         (abstract-of B x b)
         (concrete-of A l m a)
         (substitute bx b m x)
         (parallel-of ab a bx)
         (new-all P l ab)))

(define (new-all P L Q)
  (conde
   [(== P Q) (== L `())]
   [(fresh (lh lb p)
           (== L `(,lh . ,lb))
           (new-all p lb Q)
           (new-of P lh p))]))

;T|-P->^ac A:S

(public-define (comm-process T P ac A S)
               (conde
                [(comm-in T P ac A S)]
                [(comm-out T P ac A S)]
                [(comm-inter1 T P ac A S)]
                [(comm-inter2 T P ac A S)]
                [(comm-par-left T P ac A S)]
                [(comm-par-right T P ac A S)]
                [(comm-res T P ac A S)]
                ))


(define (comm-in T P ac A S)
  (fresh (a n x p s l)
          (calc-type-of T a l)
          (input--of P n x p)
          (name n)
          (action-in-of ac n)
          (abstract-of A x p)
          (cost-store-of s x)
          (cost-ass-of S a s)
          ))

(define (comm-out T P ac A S)
  (fresh (a N n p v c l)
         (calc-type-of T a l)
         (output--of P n N p)
         (name n)
         (action-out-of ac n)
         (concrete-of A `() v p)
         (eval-term N v c)
         (cost-ass-of S a c)
         (super-free-variable-value v `())
         ))


(define (comm-inter1 T P ac A S)
  (fresh (a b p q f c r s n inn outn)
         (composite-type-of T a b)
         (parallel-of P p q)
         (== ac action-none)
         (action-in-of inn n)
         (action-out-of outn n)
         (comm-process a p outn f r)
         (comm-process b q inn c s)
         (inter A f c)
         ;(== A `(@inter ,f ,c))
         (cost-ass-add S r s)
         ;(map-add (lambda (x y z) (== x `(+ ,y ,z))) S r s)
         ;(== S (list `+ r s))
         ))


(define (comm-inter2 T P ac A S)
  (fresh (a b p q f c r s n inn outn)
         (composite-type-of T a b)
         (parallel-of P p q)
         (== ac action-none)
         (action-in-of inn n)
         (action-out-of outn n)
         (comm-process a p inn c r)
         (comm-process b q outn f s)
         (inter A c f)
         (cost-ass-add S r s)))

(define (comm-par-left T P ac A S)
  (fresh (t u p q a)
         (composite-type-of T t u)
         (parallel-of P p q)
         (comm-process t p ac a S)
         (agent-parallel A a q)
         ;(== A (list `@agent-parallel a q))
         ))


(define (comm-par-right T P ac A S)
  (fresh (t u p q a)
         (composite-type-of T t u)
         (parallel-of P p q)
         (comm-process u q ac a S)
         (agent-parallel A p a)
         ;(== A (list `@agent-parallel p a))
         ))

(define (comm-res T P ac A S)
  (fresh (m p a inm outm)
         (new-of P m p)
         (comm-process T p ac a S)
         (agent-new A m a)
         (action-in-of inm m)
         (action-out-of outm m)
         (=/= ac inm)
         (=/= ac outm)))


;A |- P ->> Q :S
(public-define (mcomm-process A P Q S)
               (conde
                [(mcomm-single A P Q S)]
                [(mcomm-trans A P Q S)]
                [(mcomm-before-mred A P Q S)]
                [(mcomm-after-mred A P Q S)]))

(define (mcomm-single A P Q S)
  (comm-process A P action-none Q S))

(define (mcomm-trans A P Q S)
  (fresh (B R s1 s2)
         (type-eq A B)
         (cost-ass-add S s1 s2)
         (mcomm-process A P R s1)
         (mcomm-process B R Q s2)))


(define (mcomm-before-mred A P Q S)
  (fresh (B R s1 s2)
         (type-eq A B)
         (cost-ass-add S s1 s2)
         (mcomm-process A P R s1)
         (mred-process B R Q s2)))

(define (mcomm-after-mred A P Q S)
  (fresh (B R s1 s2)
         (type-eq A B)
         (cost-ass-add S s1 s2)
         (mred-process A P R s1)
         (mcomm-process B R Q s2)))


