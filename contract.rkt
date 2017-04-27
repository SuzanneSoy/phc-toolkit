#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (require racket/contract
           (for-syntax syntax/parse
                       racket/contract))
  
  (provide define-for-syntax/contract?
           define/contract?
           regexp-match/c
           id/c)

  (begin-for-syntax
    (define-splicing-syntax-class freevar
      (pattern {~and {~or {~seq #:freevar id contract-expr}
                          {~seq #:freevars ([ids contract-exprs] ...)}
                          {~seq}}
                     {~seq fv ...}})))

  (begin-for-syntax
    (define enable-contracts (make-parameter #t)))

  (define-syntax define-for-syntax/contract?
    (syntax-parser
      [(_ id/head contract-expr fv:freevar . body)
       (if (enable-contracts)
           #'(begin-for-syntax
               (define/contract id/head contract-expr fv.fv ... . body))
           #'(define-for-syntax id/head . body))]))

  (define-syntax define/contract?
    (syntax-parser
      [(_ id/head contract-expr fv:freevar . body)
       (if (enable-contracts)
           #'(define/contract id/head contract-expr fv.fv ... . body)
           #'(define id/head . body))]))

  (module m-contracts racket/base
    (require racket/contract)

    (provide regexp-match/c
             id/c)
    
    (define (regexp-match/c rx)
      (and/c (or/c string? bytes? path? input-port?)
             (λ (s) (regexp-match? rx s))))
  
    (define (id/c id)
      (and/c identifier? (λ (i) (free-identifier=? i id)))))
  
  (require 'm-contracts))