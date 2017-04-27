#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  ;; TODO: these won't expand types in the ann.
  (provide check-equal?:
           check-eq?:
           check-true:
           check-not-false:
           check-false:
           check-not-equal?:
           check-exn:
           check-not-exn:)
  
  (require "typed-untyped.rkt"
           (for-syntax type-expander/expander))
  
  (require/typed rackunit
                 [(check-true untyped:check-true)
                  (->* (Any) (String) Any)]
                 [(check-exn untyped:check-exn)
                  (->* ((U Regexp (→ Any Any)) (→ Any)) (String) Any)]
                 [(check-not-exn untyped:check-not-exn)
                  (->* ((→ Any)) (String) Any)]
                 [#:struct check-info ([name : Symbol] [value : Any])]
                 [make-check-info (→ Symbol Any check-info)]
                 [make-check-location (→ (List Any
                                               (U Number False)
                                               (U Number False)
                                               (U Number False)
                                               (U Number False))
                                         check-info)]
                 [make-check-name (→ Any check-info)]
                 [make-check-params (→ Any check-info)]
                 [make-check-actual (→ Any check-info)]
                 [make-check-expected (→ Any check-info)]
                 [make-check-expression (→ Any check-info)]
                 [make-check-message (→ Any check-info)]
                 [with-check-info* (→ (Listof check-info) (→ Any) Any)])
  (require (only-in typed/rackunit check-exn check-not-exn))
  
  (require (for-syntax syntax/parse
                       syntax/parse/experimental/template))
  (require-typed/untyped "syntax-parse.rkt")

  (define-syntax/parse
      (check-equal?: actual
                     (~optional (~seq (~datum :) type:type-expand!))
                     expected
                     (~optional message:expr))
    (quasitemplate
     (with-check-info* (list (make-check-actual (format "~s" actual))
                             (make-check-expected (format "~s" expected))
                             (make-check-name 'check-equal?:)
                             (make-check-params
                              (format "~s" `(,actual (?? 'type) ,expected)))
                             (make-check-location '(#,(syntax-source stx)
                                                    #,(syntax-line stx)
                                                    #,(syntax-column stx)
                                                    #,(syntax-position stx)
                                                    #,(syntax-span stx)))
                             (make-check-expression '#,(syntax->datum stx)))
                       (λ ()
                         (untyped:check-true
                          (equal? (?? (ann actual type.expanded) actual)
                                  expected))))))

  ;; TODO: factor out some of this code.
  (define-syntax/parse
      (check-eq?: actual
                  (~optional (~seq (~datum :) type:type-expand!))
                  expected
                  (~optional message:expr))
    (quasitemplate
     (with-check-info* (list (make-check-actual (format "~s" actual))
                             (make-check-expected (format "~s" expected))
                             (make-check-name 'check-eq?:)
                             (make-check-params
                              (format "~s" `(,actual (?? 'type) ,expected)))
                             (make-check-location '(#,(syntax-source stx)
                                                    #,(syntax-line stx)
                                                    #,(syntax-column stx)
                                                    #,(syntax-position stx)
                                                    #,(syntax-span stx)))
                             (make-check-expression '#,(syntax->datum stx)))
                       (λ ()
                         (untyped:check-true
                          (eq? (?? (ann actual type.expanded) actual)
                               expected))))))

  (define-syntax-rule (define-check-1 name process)
    (define-syntax/parse (name actual (~optional message:expr))
      (quasitemplate
       (with-check-info* (list (make-check-actual (format "~s" actual))
                               (make-check-expected (format "~s" #t))
                               (make-check-name 'name)
                               (make-check-params
                                (format "~s" `(,actual)))
                               (make-check-location '(#,(syntax-source stx)
                                                      #,(syntax-line stx)
                                                      #,(syntax-column stx)
                                                      #,(syntax-position stx)
                                                      #,(syntax-span stx)))
                               (make-check-expression '#,(syntax->datum stx)))
                         (λ ()
                           (untyped:check-true (process actual)))))))

  (define-check-1 check-true: identity)
  (define-check-1 check-not-false: (λ (v) (not (not v))))
  (define-check-1 check-false: not)

  (define-syntax/parse
      (check-not-equal?: actual
                         (~optional (~seq (~datum :) type:type-expand!))
                         expected
                         (~optional message))
    (quasitemplate
     (with-check-info* (list (make-check-actual (format "~s" actual))
                             (make-check-expected (format "~s" expected))
                             (make-check-name 'check-not-equal?:)
                             (make-check-params
                              (format "~s" `(,actual (?? 'type) ,expected)))
                             (make-check-location '(#,(syntax-source stx)
                                                    #,(syntax-line stx)
                                                    #,(syntax-column stx)
                                                    #,(syntax-position stx)
                                                    #,(syntax-span stx)))
                             (make-check-expression '#,(syntax->datum stx)))
                       (λ ()
                         (untyped:check-true
                          (not (equal? (?? (ann actual type.expanded) actual)
                                       expected)))))))

  (define-syntax/parse
      (check-exn: exn-predicate-or-regexp:expr
                  thunk
                  (~optional message:expr))
    (quasitemplate
     (with-check-info* (list (make-check-name 'check-eq?:)
                             (make-check-location '(#,(syntax-source stx)
                                                    #,(syntax-line stx)
                                                    #,(syntax-column stx)
                                                    #,(syntax-position stx)
                                                    #,(syntax-span stx)))
                             (make-check-params
                              (list exn-predicate-or-regexp thunk))
                             (?? (make-check-message message))
                             (make-check-expression '#,(syntax->datum stx)))
                       (λ ()
                         (untyped:check-exn
                          exn-predicate-or-regexp
                          thunk
                          (?? message))))))

  (define-syntax/parse
      (check-not-exn: thunk
                      (~optional message:expr))
    (quasitemplate
     (with-check-info* (list (make-check-name 'check-eq?:)
                             (make-check-location '(#,(syntax-source stx)
                                                    #,(syntax-line stx)
                                                    #,(syntax-column stx)
                                                    #,(syntax-position stx)
                                                    #,(syntax-span stx)))
                             (make-check-params
                              (list thunk))
                             (?? (make-check-message message))
                             (make-check-expression '#,(syntax->datum stx)))
                       (λ ()
                         (untyped:check-not-exn
                          thunk
                          (?? message)))))))