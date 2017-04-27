#lang racket

(require rackunit
         (for-syntax phc-toolkit/untyped
                     racket/syntax
                     racket/string
                     racket/function
                     rackunit)
         (for-meta 2 racket/base)
         (for-meta 2 phc-toolkit/untyped))

(define-syntax (foo stx)
  (syntax-case stx ()
    [(_ a b)
     (let ()
       (define/with-syntax a-b (format-id #'a "~a-~a" #'a #'b))
       ;#'(define a-b 42)
       #'(inject-sub-range-formats ([#'a "~a-~a" #'a #'b])
                                   (define a-b 42)))]))

(foo x y)

;; The arrows are properly drawn here.
(check-equal? x-y 42)

(define-syntax (bar stx)
  (syntax-case stx ()
    [(_ a b)
     (let ()
       (define/with-syntax a-b (format-id #'a "~a-~a" #'a #'b))
       #'(begin-for-syntax
           (inject-sub-range-formats ([#'a "~a-~a" #'a #'b])
                                     (define a-b 42))))]))

(bar x y)

;; The arrows are properly drawn here.
(begin-for-syntax (check-equal? x-y 42))

(define-syntax (baz stx)
  (syntax-case stx ()
    [(_ a b)
     (with-format-ids/inject-binders
      ([a-b #'a "~a-~a" #'a #'b])
      #'(begin-for-syntax
          (inject-sub-range-binders ...
           (define a-b 42))))]))

(baz x z)

;; The arrows are properly drawn here.
(begin-for-syntax (check-equal? x-z 42))

(define-syntax (test-hyphen-let stx)
  (syntax-case stx ()
    [(_ [a b c] d e)
     (with-format-ids/inject-binders
      ([abc #'a "~a-~a-~a" #'a #'b #'c]
       [ac #'a "~a++~a" #'a #'c])
      #`(let ()
          (inject-sub-range-binders ...
           (define abc 123)
           (define ac 456)
           (check-equal? d 123)
           (check-equal? e 456))))]))

;; The arrows are properly drawn here.
(test-hyphen-let [a b c]
                 a-b-c a++c)
