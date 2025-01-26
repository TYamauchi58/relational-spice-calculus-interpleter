#lang racket
(require "../faster-minikanren/mk.rkt")
(require "public-define.rkt")
(require "mklist.rkt")
(require "value.rkt")
(require "term.rkt")


(public-define (env-extn-of F V x E)
               (== F `((,V ,x) . ,E)))

(public-define (environment E)
               (conde
                [(== E `())]
                [(fresh (Ev Ex Eb) (env-extn-of E Ev Ex Eb) (value Ev) (variable Ex) (environment Eb))]))


(public-define (remove-env F x E)
               (fresh (Ev Ex Eb Fb) (env-extn-of E Ev Ex Eb)
                (conde
                 [(== Ex x) (== Eb F)]
                 [(=/= Ex x) (remove-env Fb x Eb) (env-extn-of F Ev Ex Fb)])))

(public-define (lookup V x E)
               (fresh (Ev Ex Eb) (env-extn-of E Ev Ex Eb)
                (conde
                [(== Ex x) (== Ev V)]
                [(=/= Ex x) (lookup V x Eb)])))

;FREE NAME

(public-define (not-free-name-environment E N)
               (conde
                [(== E `())]
                [(fresh (Ev Ex Eb) (== E `((,Ev ,Ex) . ,Eb))
                        (not-free-name-value Ev N)
                        (not-free-name-environment Eb N))]))
