#lang racket/base

(provide in)

(require racket/stxparam
         (for-syntax racket/base))

(define-syntax-parameter in
  (λ (stx)
    (raise-syntax-error
     'in
     "used out of context. It can only be used in some forms."
     stx)))