#lang racket
(require "../faster-minikanren/mk.rkt")
(require "public-define.rkt")
(require "value.rkt")
(require "term.rkt")
(require "process.rkt")
(require "environment.rkt")
(require "envproc.rkt")
(require "agent.rkt")
(require "typing.rkt")
(require "cost.rkt")
(require "eval.rkt")
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
  (fresh (x a l m b ax ab ad p)
         (abstract-of A x ad a)
         (concrete-of B l m b)
         (subst-envproc-addr ax a m x ad)
         (parallel-envproc-of ab ax b)
         (new-all p l ab)
         (process-agent-of P p)))

(define (inter2 P A B)
  (fresh (x a l m b bx ab ad p)
         (abstract-of B x ad b)
         (concrete-of A l m a)
         (subst-envproc-addr bx b m x ad)
         (parallel-envproc-of ab a bx)
         (new-all p l ab)
         (process-agent-of P p)))


(public-define (subst-envproc-addr Q P V x ad)
               (conde
                [(fresh (p e f) (== ad `here) (env-app-of P p e) (env-app-of Q p f) (env-extn-of f V x e))]
                [(fresh (p1 p2 q1 ad1) (== ad `(,ad1 ())) (parallel-envproc-of P p1 p2) (parallel-envproc-of Q q1 p2)
                        (subst-envproc-addr q1 p1 V x ad1))]
                [(fresh (p1 p2 q2 ad1) (== ad `(() ,ad1)) (parallel-envproc-of P p1 p2) (parallel-envproc-of Q p1 q2)
                        (subst-envproc-addr q2 p2 V x ad1))]
                [(fresh (p n q) (new-envproc-of P n p) (new-envproc-of Q n q) (subst-envproc-addr q p V x ad))]))


(define (new-all P L Q)
  (conde
   [(== P Q) (== L `())]
   [(fresh (lh lb p)
           (== L `(,lh . ,lb))
           (new-all p lb Q)
           (new-envproc-of P lh p))]))

;T|-P->^ac A:S
(public-define (comm-process T P ac A S)
               (fresh (r)
                      (comm-process@ r P ac A S)
                      (type-represent r T)))

(public-define (comm-process@ T P ac A S)
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
  (fresh (a t n x ip p ep e s c)
          (calc-type-class-of T a)
          (env-app-of P ip e)
          (input--of ip t x p)
          (env-app-of ep p e)
          (eval-term e t n c)
          (name n)
          (action-in-of ac n)
          (abstract-of A x `here ep)
          (cost-ass-of S a c)
          ))

(define (comm-out T P ac A S)
  (fresh (a N t n op p e v ct c c2 ep)
         (calc-type-class-of T a)
         (env-app-of P op e)
         (output--of op t N p)
         (eval-term e t n ct)
         (name n)
         (action-out-of ac n)
         (env-app-of ep p e)
         (concrete-of A `() v ep)
         (eval-term e N v c)
         (cost-add c2 c ct)
         (cost-ass-of S a c2)
         ))


(define (comm-inter1 T P ac A S)
  (fresh (a b p q f c r s n inn outn)
         (type-represent-composition T a b)
         (parallel-envproc-of P p q)
         (== ac action-none)
         (action-in-of inn n)
         (action-out-of outn n)
         (comm-process@ a p outn f r)
         (comm-process@ b q inn c s)
         (inter A f c)
         ;(== A `(@inter ,f ,c))
         (cost-ass-add S r s)
         ;(map-add (lambda (x y z) (== x `(+ ,y ,z))) S r s)
         ;(== S (list `+ r s))
         ))


(define (comm-inter2 T P ac A S)
  (fresh (a b p q f c r s n inn outn)
         (type-represent-composition T a b)
         (parallel-envproc-of P p q)
         (== ac action-none)
         (action-in-of inn n)
         (action-out-of outn n)
         (comm-process@ a p inn c r)
         (comm-process@ b q outn f s)
         (inter A c f)
         (cost-ass-add S r s)))

(define (comm-par-left T P ac A S)
  (fresh (t u p q a)
         (type-represent-composition T t u)
         (parallel-envproc-of P p q)
         (comm-process@ t p ac a S)
         (agent-parallel A a q)
         ;(== A (list `@agent-parallel a q))
         ))


(define (comm-par-right T P ac A S)
  (fresh (t u p q a)
         (type-represent-composition T t u)
         (parallel-envproc-of P p q)
         (comm-process@ u q ac a S)
         (agent-parallel A p a)
         ;(== A (list `@agent-parallel p a))
         ))

(define (comm-res T P ac A S)
  (fresh (m p a inm outm)
         (new-envproc-of P m p)
         (comm-process@ T p ac a S)
         (agent-new A m a)
         (action-in-of inm m)
         (action-out-of outm m)
         (=/= ac inm)
         (=/= ac outm)))


;A |- P ->> Q :S
(public-define (mcomm-process A P Q S)
               (fresh (r)
                      (mcomm-process@ r P Q S)
                      (type-represent r A)))

(public-define (mcomm-process@ A P Q S)
               (fresh (p1 p2 pp2 s1 s2 s3 s4)
                        (mred-process@ A P p1 s1)
                        (comm-process@ A p1 action-none pp2 s2)
                        (process-agent-of pp2 p2)
                        (cost-ass-add s3 s2 s1)
                        (conde
                         [(mcomm-process@ A p2 Q s4)]
                         [(mred-process@ A p2 Q s4)])
                        (cost-ass-add S s4 s3)
                        ))
