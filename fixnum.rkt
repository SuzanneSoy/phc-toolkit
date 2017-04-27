#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (provide fxxor fxxor2)
  
  ;; For fxxor, used to compute hashes.
  ;; The type obtained just by writing (require racket/fixnum) is wrong, so we
  ;; get a more precise one.
  (require/typed racket/fixnum [(fxxor fxxor2) (→ Fixnum Fixnum Fixnum)])
  
  (: fxxor (→ Fixnum * Fixnum))
  (define (fxxor . args)
    (foldl fxxor2 0 args)))