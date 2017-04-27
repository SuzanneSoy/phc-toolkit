#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules
  (provide first-value second-value third-value fourth-value fifth-value
           sixth-value seventh-value eighth-value ninth-value tenth-value
           cons→values
           (rename-out [cons→values cons->values]))
  
  (define-syntax-rule (define-value-getter name accessor)
    (define-syntax-rule (name expr)
      ;; Using just (call-with values (λ () expr) accessor) does not work well
      ;; when expr returns AnyValues (tested with eval below).
      (call-with-values (λ () expr) (λ vs (accessor vs)))))
  
  (define-value-getter first-value   first)
  (define-value-getter second-value  second)
  (define-value-getter third-value   third)
  (define-value-getter fourth-value  fourth)
  (define-value-getter fifth-value   fifth)
  (define-value-getter sixth-value   sixth)
  (define-value-getter seventh-value seventh)
  (define-value-getter eighth-value  eighth)
  (define-value-getter ninth-value   ninth)
  (define-value-getter tenth-value   tenth)
  
  (module+ test
    (require typed/rackunit)
    (check-equal? (first-value   (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 1)
    (check-equal? (second-value  (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 2)
    (check-equal? (third-value   (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 3)
    (check-equal? (fourth-value  (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 4)
    (check-equal? (fifth-value   (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 5)
    (check-equal? (sixth-value   (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 6)
    (check-equal? (seventh-value (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 7)
    (check-equal? (eighth-value  (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 8)
    (check-equal? (ninth-value   (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 9)
    (check-equal? (tenth-value   (values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)) 10)
    ;; eval returns AnyValues, which behaves differently
    (let ([ev '(values 1 2 3 4 5 6 7 8 9 10 11 12 13 14)])
      (check-equal? (first-value   (eval ev)) 1)
      (check-equal? (second-value  (eval ev)) 2)
      (check-equal? (third-value   (eval ev)) 3)
      (check-equal? (fourth-value  (eval ev)) 4)
      (check-equal? (fifth-value   (eval ev)) 5)
      (check-equal? (sixth-value   (eval ev)) 6)
      (check-equal? (seventh-value (eval ev)) 7)
      (check-equal? (eighth-value  (eval ev)) 8)
      (check-equal? (ninth-value   (eval ev)) 9)
      (check-equal? (tenth-value   (eval ev)) 10)))
  
  (define #:∀ (A B) (cons→values [x : (Pairof A B)]) (values (car x) (cdr x))))