#lang racket
(require "../faster-minikanren/mk.rkt")
(require "process.rkt")
(require "commit.rkt")
(require "reduction.rkt")


(define (list->csvstring lines)
  (define (list-csvline l)
    (string-join (map number->string l) ","))
  (string-join (cons "n,time(ms)"  (map list-csvline lines)) "\n"))


(define (write-csv lists)
  (begin
    (define fout (open-output-file "spice-time-vs-slow.csv" #:exists `truncate))
    (display (list->csvstring lists) fout)
    (close-output-port fout)
    `finish!))


(define (time-of func)
  (define N 4)
  (define (ntimes func n)
    (cond
      ((= n 0) `())
      (else (begin (func) (ntimes func (- n 1))))))
  (let ((start (current-inexact-milliseconds)))
    (begin (ntimes func N) (/ (- (current-inexact-milliseconds) start) N))))

(define (testcase-n-io n)
  (define (testcase-n-in n)
    (cond
      ((= n 0) `())
      (else `((inp (name N) (variable ,n)) . ,(testcase-n-in (- n 1))))))
  (define (testcase-n-out n)
    (cond
      ((= n 0) `())
      (else `((out (name N) (constant ,n)) . ,(testcase-n-out (- n 1))))))
  `(parallel ,(testcase-n-in n) ,(testcase-n-out n)))

(define (time-test n)
  (let ((testcase (testcase-n-io n)))
    (time-of (lambda () (run* (x) (fresh (S)
                                       (mcomm-process@
                                        `(composite-type-class
                                          (calc-type-class (calc Cp))
                                          (calc-type-class (calc Cq)))
                                        testcase
                                        x
                                        S)))))))


(define (time-test2 n)
  (let ((testcase (testcase-n-io n)))
    (time-of (lambda () (run 1 (x)
                                       (mcomm-process
                                        `(composite-type
                                          (calc-type (calc Cp) ())
                                          (calc-type (calc Cq) ()))
                                        testcase
                                        `(parallel () ())
                                        x))))))


(define (time-test-pow n)
  (define (time-test-pow-sub n m)
    (cond
      ((< n m) `())
      (else (cons (list m (time-test m)) (time-test-pow-sub n (* m 2))))))
  (time-test-pow-sub n 1))

(define (time-test-seq n)
  (define (time-test-seq-sub n m)
    (cond
      ((< n m) `())
      (else (cons (list m (time-test2 m)) (time-test-seq-sub n (+ m 1))))))
  (time-test-seq-sub n 1))

(define (test-s)
  (run 1 (x)
       (mcomm-process
        `(composite-type
          (calc-type (calc Cp) ())
          (calc-type (calc Cq) ()))
        `(parallel
          ((inp (name N) (variable x)) (inp (name N) (variable Y)))
          ((out (name N) (constant 1)) (out (name N) (constant 1))))
        `(parallel () ())
        x)))



;3way handshake
(define (test-3way-handshake)
  (define P `((new (name Sa))
              (store (variable Xsa) (name Sa))
              (out (name C) (pair-term (constant A) (pair-term (constant B) (variable Xsa))))
              (inp (name C) (variable Bp-Ap-Sbp-sXsap))
              (split ((variable Bp) (variable Ap-Sbp-sXsap)) (variable Bp-Ap-Sbp-sXsap) ((free (variable Bp-Ap-Sbp-sXsap)) (free (variable Xsa))))
              (free (variable Bp-Ap-Sbp-sXsap))
              (split ((variable Ap) (variable Sbp-sXsap)) (variable Ap-Sbp-sXsap) ((free (variable Ap-Sbp-sXsap)) (free (variable Bp)) (free (variable Xsa))))
              (free (variable Ap-Sbp-sXsap))
              (split ((variable Sbp) (variable sXsap)) (variable Sbp-sXsap) ((free (variable Sbp-sXsap)) (free (variable Ap)) (free (variable Bp)) (free (variable Xsa))))
              (free (variable Sbp-sXsap))
              (match (variable Ap) (constant A) ((free (variable Ap)) (free (variable Bp)) (free (variable Sbp)) (free (variable sXsap)) (free (variable Xsa))))
              (match (variable Bp) (constant B) ((free (variable Ap)) (free (variable Bp)) (free (variable Sbp)) (free (variable sXsap)) (free (variable Xsa))))
              (match (variable sXsap) (pair-term (variable Xsa) (constant 1)) ((free (variable Ap)) (free (variable Bp)) (free (variable Sbp)) (free (variable sXsap)) (free (variable Xsa))))
              (free (variable Ap))
              (free (variable Bp))
              (free (variable sXsap))
              (out (name C) (pair-term (constant A) (pair-term (constant B) (pair-term (pair-term (variable Xsa) (constant 1)) (pair-term (variable Sbp) (constant 1))))))
              ))
  (define Q `((new (name Sb))
              (inp (name C) (variable Aq-Bq-Xsaq))
              (split ((variable Aq) (variable Bq-Xsaq)) (variable Aq-Bq-Xsaq) ((free (variable Aq-Bq-Xsaq))))
              (free (variable Aq-Bq-Xsaq))
              (split ((variable Bq) (variable Xsaq)) (variable Bq-Xsaq) ((free (variable Bq-Xsaq))))
              (free (variable Bq-Xsaq))
              (match (variable Bq) (constant B) ((free (variable Bq)) (free (variable Aq)) (free (variable Xsaq))))
              (store (variable Sbq) (name Sb))
              (out (name C) (pair-term (constant B) (pair-term (variable Aq) (pair-term (variable Sbq) (pair-term (variable Xsaq) (constant 1))))))
              (inp (name C) (variable Aq2-Bq2-sSaq-sSbq))
              (split ((variable Aq2) (variable Bq2-sSaq-sSbq)) (variable Aq2-Bq2-sSaq-sSbq) ((free (variable Aq)) (free (variable Bq)) (free (variable Aq2-Bq2-sSaq-sSbq)) (free (variable Sbq))))
              (free (variable Aq2-Bq2-sSaq-sSbq))
              (split ((variable Bq2) (variable sSaq-sSbq)) (variable Bq2-sSaq-sSbq) ((free (variable Aq)) (free (variable Bq)) (free (variable Aq2)) (free (variable Bq2-sSaq-sSbq)) (free (variable Sbq))))
              (free (variable Bq2-sSaq-sSbq))
              (split ((variable sSaq) (variable sSbq)) (variable sSaq-sSbq) ((free (variable Aq)) (free (variable Bq)) (free (variable Aq2)) (free (variable Bq2)) (free (variable sSaq-sSbq)) (free (variable Sbq))))
              (free (variable sSaq-sSbq))
              (match (variable Aq) (variable Aq2) ((free (variable Aq)) (free (variable Bq)) (free (variable Aq2)) (free (variable Bq2)) (free (variable sSaq)) (free (variable sSbq)) (free (variable Sbq))))
              (match (variable Bq) (constant B) ((free (variable Aq)) (free (variable Bq)) (free (variable Aq2)) (free (variable Bq2)) (free (variable sSaq)) (free (variable sSbq)) (free (variable Sbq))))
              (match (variable sSaq) (pair-term (variable Xsaq) (constant 1)) ((free (variable Aq)) (free (variable Bq)) (free (variable Aq2)) (free (variable Bq2)) (free (variable sSaq)) (free (variable sSbq)) (free (variable Sbq))))
              (match (variable sSbq) (pair-term (variable Sbq) (constant 1)) ((free (variable Aq)) (free (variable Bq)) (free (variable Aq2)) (free (variable Bq2)) (free (variable sSaq)) (free (variable sSbq)) (free (variable Sbq))))
              (free (variable Aq2))
              (free (variable Bq))
              (free (variable sSaq))
              (free (variable sSbq))
              ))
  (define testcase `(parallel ,P ,Q))
  (define type-2calc `(composite-type-class (calc-type-class (calc C1)) (calc-type-class (calc C2))))
  (run* (x) (fresh (fin S) (== x `(,type-2calc /= ,testcase ->> ,fin : ,S)) (mcomm-process@ type-2calc testcase fin S))))

;SYN-flood
(define (test-syn-flood)
  (define Q `((new (name Sb))
              (inp (name C) (variable Aq-Bq-Xsaq))
              (split ((variable Aq) (variable Bq-Xsaq)) (variable Aq-Bq-Xsaq) ((free (variable Aq-Bq-Xsaq))))
              (free (variable Aq-Bq-Xsaq))
              (split ((variable Bq) (variable Xsaq)) (variable Bq-Xsaq) ((free (variable Bq-Xsaq))))
              (free (variable Bq-Xsaq))
              (match (variable Bq) (constant B) ((free (variable Bq)) (free (variable Aq)) (free (variable Xsaq))))
              (store (variable Sbq) (name Sb))
              (out (name C) (pair-term (constant B) (pair-term (variable Aq) (pair-term (variable Sbq) (pair-term (variable Xsaq) (constant 1))))))
              (inp (name C) (variable Aq2-Bq2-sSaq-sSbq))
              (split ((variable Aq2) (variable Bq2-sSaq-sSbq)) (variable Aq2-Bq2-sSaq-sSbq) ((free (variable Aq)) (free (variable Bq)) (free (variable Aq2-Bq2-sSaq-sSbq)) (free (variable Sbq))))
              (free (variable Aq2-Bq2-sSaq-sSbq))
              (split ((variable Bq2) (variable sSaq-sSbq)) (variable Bq2-sSaq-sSbq) ((free (variable Aq)) (free (variable Bq)) (free (variable Aq2)) (free (variable Bq2-sSaq-sSbq)) (free (variable Sbq))))
              (free (variable Bq2-sSaq-sSbq))
              (split ((variable sSaq) (variable sSbq)) (variable sSaq-sSbq) ((free (variable Aq)) (free (variable Bq)) (free (variable Aq2)) (free (variable Bq2)) (free (variable sSaq-sSbq)) (free (variable Sbq))))
              (free (variable sSaq-sSbq))
              (match (variable Aq) (variable Aq2) ((free (variable Aq)) (free (variable Bq)) (free (variable Aq2)) (free (variable Bq2)) (free (variable sSaq)) (free (variable sSbq)) (free (variable Sbq))))
              (match (variable Bq) (constant B) ((free (variable Aq)) (free (variable Bq)) (free (variable Aq2)) (free (variable Bq2)) (free (variable sSaq)) (free (variable sSbq)) (free (variable Sbq))))
              (match (variable sSaq) (pair-term (variable Xsaq) (constant 1)) ((free (variable Aq)) (free (variable Bq)) (free (variable Aq2)) (free (variable Bq2)) (free (variable sSaq)) (free (variable sSbq)) (free (variable Sbq))))
              (match (variable sSbq) (pair-term (variable Sbq) (constant 1)) ((free (variable Aq)) (free (variable Bq)) (free (variable Aq2)) (free (variable Bq2)) (free (variable sSaq)) (free (variable sSbq)) (free (variable Sbq))))
              (free (variable Aq2))
              (free (variable Bq))
              (free (variable sSaq))
              (free (variable sSbq)))
    )
  (define I `((new (name i)) (new (name s)) (out (name C) (pair-term (name i) (pair-term (constant B) (name s)))))
              )
  (define R `((inp (name C) (variable r)))
              )
  (define testcase `(parallel ,Q (parallel ,I ,R)))
  (define type-3calc `(composite-type-class (calc-type-class (calc C1)) (composite-type-class (calc-type-class (calc C2))(calc-type-class (calc C3)))))
  (run* (x) (fresh (fin S) (== x `(,type-3calc /= ,testcase ,fin : ,S)) (mcomm-process@ type-3calc testcase fin S))))


(define (test-1comm)
  (run 1 (Q)
       (fresh (S)
              (mcomm-process
               `(composite-type
                 (calc-type (calc Cp) ())
                 (calc-type (calc Cq) ())
                 )
               `(parallel
                 ((inp (name N) (variable x)))
                 ((out (name N) (constant 1)))
                 )
               Q S
               ))))

