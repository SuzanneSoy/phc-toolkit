#lang racket/base

(require (for-syntax racket/base)
         phc-toolkit/meta-struct
         rackunit)

(define-syntax (test-subtype? stx)
  (syntax-case stx ()
    [(_ sub super)
     #`#,(if (meta-struct-subtype? #'sub #'super)
             #t
             #f)]))

(module m1 racket
  (struct sa ())
  (provide (struct-out sa)))
(module m2 racket
  (require (submod ".." m1))
  (struct sb sa ())
  (provide (rename-out [sa sa2]))
  (provide (struct-out sb)))
(require 'm1)
(require 'm2)
(struct sc sb ())

(check-true (test-subtype? sa sa))
(check-true (test-subtype? sa2 sa))
(check-true (test-subtype? sb sa))
(check-true (test-subtype? sc sa))

(check-true (test-subtype? sa sa2))
(check-true (test-subtype? sa2 sa2))
(check-true (test-subtype? sb sa2))
(check-true (test-subtype? sc sa2))

(check-false (test-subtype? sa sb))
(check-false (test-subtype? sa2 sb))
(check-true (test-subtype? sb sb))
(check-true (test-subtype? sc sb))

(check-false (test-subtype? sa sc))
(check-false (test-subtype? sa2 sc))
(check-false (test-subtype? sb sc))
(check-true (test-subtype? sc sc))





(struct s (f) #:mutable)
(struct t s (g))
(struct u (f))
(struct v u (g))
(begin-for-syntax
  (require rackunit)
  (check-false (struct-type-id-is-immutable? #'s))
  (check-false (struct-type-id-is-immutable? #'t))
  (check-true (struct-type-id-is-immutable? #'u))
  (check-true (struct-type-id-is-immutable? #'v)))

(struct ts (f) #:mutable #:transparent)
(struct tt ts (g) #:transparent)
(struct tu ([f #:mutable] g h) #:transparent)
(struct tv tu (i j k l) #:transparent)
(struct tw (f g h) #:transparent)
(struct tx tu (i j k l) #:transparent)
  
(require rackunit)
(check-false (struct-instance-is-immutable? (s 1)))
(check-false (struct-instance-is-immutable? (t 1 2)))
;; can't tell for u, because the struct is opaque.
(check-false (struct-instance-is-immutable? (u 1)))

(check-false (struct-instance-is-immutable? (ts 1)))
(check-false (struct-instance-is-immutable? (tt 1 2)))
(check-false (struct-instance-is-immutable? (tv 1 2 3 4 5 6 7)))
(check-false (struct-instance-is-immutable? (tu 1 2 3)))
(check-true (struct-instance-is-immutable? (tw 1 2 3)))
(check-false (struct-instance-is-immutable? (tx 1 2 3 4 5 6 7)))