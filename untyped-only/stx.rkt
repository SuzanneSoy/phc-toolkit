#lang racket/base

(require (for-template racket/base)
         (for-syntax racket/base)
         "../stx/fold.rkt")

(provide make-rest-transformer
         make-id+call-transformer
         make-id+call-transformer-delayed)

(define (make-rest-transformer f)
  (λ (stx)
    (syntax-case stx ()
      [(_ . rest) (f #'rest)])))

(define (make-id+call-transformer-delayed stx-value)
  (λ (stx)
    (syntax-case stx ()
      [(_ . args) (quasisyntax/top-loc stx (#,(stx-value) . args))]
      [id (identifier? #'id) (stx-value)])))

(define (make-id+call-transformer stx-value)
  (make-id+call-transformer-delayed (λ () stx-value)))
