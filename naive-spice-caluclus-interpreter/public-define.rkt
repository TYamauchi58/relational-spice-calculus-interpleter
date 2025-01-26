#lang racket

(provide public-define)
(define-syntax public-define
  (syntax-rules ()
    [(_ (name arg ...) body) (begin (define (name arg ...) body) (provide name))]
    [(_ name body) (begin (define name body) (provide name))]
    ))