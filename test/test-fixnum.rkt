#lang typed/racket
(require "../typed-untyped.rkt")
(define-typed/untyped-test-module
  (require-typed/untyped phc-toolkit/fixnum)
  (check-equal? (fxxor2 13206 23715) 28469)
  (check-equal? (fxxor 0) 0)
  (check-equal? (fxxor 13206) 13206)
  (check-equal? (fxxor 13206 23715) 28469)
  (check-equal? (fxxor 13206 23715 314576) 304101))