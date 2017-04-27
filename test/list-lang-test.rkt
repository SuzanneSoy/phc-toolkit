#lang s-exp phc-toolkit/list-lang

(require typed/rackunit)
(check-equal? whole-list '((a 1) b c (3 4 5)))

(define-list-values whole-list : (Listof (U Symbol (Listof (U Symbol Number)))))
;; All the items below are quoted and aggregated into whole-list.
(a 1)
b

c
(3 4 5)
