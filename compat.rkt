#lang typed/racket
;; Compatibility functions for Racket version 6.5.

(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (require (only-in racket/syntax with-disappeared-uses))
  (require/typed racket/syntax
                 [record-disappeared-uses
                  ;; This is the type in v. 6.5. Later versions allow
                  ;; (U Identifier (Listof Identifier)). The wrapper below
                  ;; generalizes that type.
                  (→ (Listof Identifier) Any)])
  (provide with-disappeared-uses*
           record-disappeared-uses*)

  (define-syntax-rule (with-disappeared-uses* . body)
    (with-disappeared-uses (let () . body)))

  (: record-disappeared-uses* (→ (U Identifier (Listof Identifier)) Any))
  (define (record-disappeared-uses* ids)
    (record-disappeared-uses (if (list? ids) ids (list ids)))))
