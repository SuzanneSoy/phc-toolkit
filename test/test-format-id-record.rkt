#lang racket

(require (for-syntax "../untyped-only/format-id-record.rkt"
                     racket/syntax
                     racket/string
                     racket/function)
         rackunit)

(define-syntax (test-hyphen stx)
  (syntax-case stx ()
    [(_ [a ...] b)
     (with-sub-range-binders
      #`(begin (define #,(apply format-id/record
                                (car (syntax->list #'(a ...)))
                                (string-join (map (const "~a")
                                                  (syntax->list #'(a ...)))
                                             "-")
                                (syntax->list #'(a ...)))
                 123)
               (check-equal? b 123)))]))

(test-hyphen [a b c xyz] a-b-c-xyz)
(let ()
  (test-hyphen [a b c xyz] a-b-c-xyz))

(define-syntax (test-concat stx)
  (syntax-case stx ()
    [(_ [a b c] d)
     (with-sub-range-binders
      #`(begin (define #,(format-id/record #'a "~a~a~a" #'a #'b #'c)
                 9)
               (check-equal? d 9)))]))

(test-concat [a bb ccc] abbccc)
;; Misaligned sub-range-binders are due to
;; https://github.com/racket/drracket/issues/68
(test-concat [1 81 6561] |1816561|)
(let ()
  (test-concat [a bb ccc] abbccc)
  (test-concat [1 81 6561] |1816561|))


(define-syntax (test-arrows stx)
  (syntax-case stx ()
    [(_ [a b c] d e)
     (with-arrows
      #`(begin (define #,(format-id/record #'a "~a~a~a" #'a #'b #'c)
                 321)
               (check-equal? d #,(syntax-local-value/record #'e number?))))]))

(define-syntax the-e 321)
(test-arrows [xxx yy z] xxxyyz the-e)

(let ()
  (define-syntax the-e 321)
  (test-arrows [xxx yy z] xxxyyz the-e))

;; Does not work. I suspect that the 'sub-range-binders must have the exact same
;; scope as the bound identifier, but `let` introduces new scopes that the
;; identifiers within sub-range-binders won't have.
(define-syntax (test-hyphen-let stx)
  (syntax-case stx ()
    [(_ [a ...] b)
     #`(let ()
         #,(with-sub-range-binders
            #`(begin
                (define #,(apply format-id/record
                                 (car (syntax->list #'(a ...)))
                                 (string-join (map (const "~a")
                                                   (syntax->list #'(a ...)))
                                              "-")
                                 (syntax->list #'(a ...)))
                  123)
                (check-equal? b 123))))]))

(test-hyphen-let [a b c xyz2] a-b-c-xyz2)

(define-syntax (test-fmt stx)
  (syntax-case stx ()
    [(_ fmt [a b c] d)
     (with-sub-range-binders
      #`(begin (define #,(format-id/record #'fmt #'fmt #'a #'b #'c)
                 9)
               (check-equal? d 9)))]))

;; Draws the following arrows:
;; w→w 1→1 x~~x→x~x 2→2 y→y 3→3 z→z
;; Nothing drawn from or to the "~a" themselves.
(test-fmt "w~ax~~x~ay~az" [1 2 3] w1x~x2y3z)