#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (module m racket/base
    (provide eval-get-values)
    
    (define (eval-get-values expr [namespace (current-namespace)])
      (call-with-values (λ () (eval expr namespace)) list)))

  (require "typed-untyped.rkt")
  (if-typed
   (begin
     (require typed/racket/unsafe)
     (unsafe-require/typed 'm [eval-get-values (->* (Any) (Namespace) (Listof Any))]))
   (require 'm))
  
  (provide eval-get-values))