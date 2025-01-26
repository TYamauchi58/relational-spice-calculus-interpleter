#lang racket
(require "../faster-minikanren/mk.rkt")
(require "commit.rkt")


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


(define (time-test-1-result n)
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

