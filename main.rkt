#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (require "typed-untyped.rkt")
  (provide (all-from-out "typed-untyped.rkt"))

  (if-typed
   (begin (require "aliases.rkt")
          (provide (all-from-out "aliases.rkt")))
   (begin (require "untyped/aliases.rkt")
          (provide (all-from-out "untyped/aliases.rkt"))))
  
  ;(require/provide (typed/untyped "fixnum.rkt" …))
  (require/provide-typed/untyped
   "misc.rkt"
   "require-provide.rkt"
   "fixnum.rkt"
   "typed-rackunit.rkt"
   "typed-rackunit-extensions.rkt"
   "syntax-parse.rkt"
   "tmpl.rkt"
   "threading.rkt"
   "sequence.rkt"
   "repeat-stx.rkt"
   "stx.rkt"
   "list.rkt"
   "values.rkt"
   "ids.rkt"
   "generate-indices.rkt"
   "set.rkt"
   "type-inference-helpers.rkt"
   "percent.rkt"
   "not-implemented-yet.rkt"
   "cond-let.rkt"
   "multiassoc-syntax.rkt"
   "tmpl-multiassoc-syntax.rkt"
   "logn-id.rkt"
   "compat.rkt"
   "eval-get-values.rkt"
   "meta-struct.rkt"
   "contract.rkt")

  (when-untyped
   (require/provide "untyped/for-star-list-star.rkt"
                    "untyped/format-id-record.rkt")))