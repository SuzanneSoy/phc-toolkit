#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (provide cars cdrs lists maybe-vector->list)
  
  #|
  ;; This does not work, in the end.
  (provide imap)
  (define-syntax (imap stx)
    (syntax-parse stx
      [(_ lst:expr var:id (~optional (~literal →)) . body)
       #'(let ()
           (define #:∀ (T) (inlined-map [l : (Listof T)])
             (if (null? l)
                 '()
                 (cons (let ([var (car l)]) . body)
                       (inlined-map (cdr l)))))
           (inlined-map lst))]))
  |#
  
  (: cars (∀ (A) (→ (Listof (Pairof A Any)) (Listof A))))
  (define (cars l) ((inst map A (Pairof A Any)) car l))
  
  (: cdrs (∀ (B) (→ (Listof (Pairof Any B)) (Listof B))))
  (define (cdrs l) ((inst map B (Pairof Any B)) cdr l))

  (: lists (∀ (A) (→ (Listof A) (Listof (List A)))))
  (define (lists l) ((inst map (List A) A) (λ (x) (list x)) l))

  (module m-maybe-vector->list racket/base
    (provide maybe-vector->list)
    (define (maybe-vector->list v)
      (if (vector? v)
          (vector->list v)
          #f)))

  (require (only-in typed/racket/unsafe unsafe-require/typed)
           "typed-untyped.rkt")
  (if-typed
   (unsafe-require/typed 'm-maybe-vector->list
                         [maybe-vector->list (→ Any (U (Listof Any) #f))])
   (require 'm-maybe-vector->list))

  (when-typed
   (require type-expander)
   (provide maybe-apply-type)
   (define-type-expander (maybe-apply-type stx)
     (syntax-case stx ()
       [(_ τ) #'τ]
       [(_ τ . args) #'(τ . args)]))))