#lang racket

(require "../untyped-only/syntax-parse.rkt"
         syntax/parse
         rackunit
         syntax/macro-testing
         (for-syntax racket/match))

(check-equal? (map syntax->datum
                   (syntax-case #'(1 2 3) ()
                     [(x ...) (attribute* x)]))
              '(1 2 3))

(check-equal? (map syntax->datum
                   (syntax-parse #'(1 2 3)
                     [(x ...) (attribute* x)]))
              '(1 2 3))

(check-exn #rx"not bound as an attribute or pattern variable"
           (λ ()
             (convert-compile-time-error
              (let ([x #'(1 2 3)])
                (attribute* x)))))

(define-syntax-class stxclass
  (pattern foo))
(check-true
 (syntax-parse #'(1 2 3)
   [(a ... sc:stxclass)
    #:attr b 42
    (syntax-case #'(4 5 6) ()
      [(c ...)
       (let ()
         (define-syntax (tst stx)
           #`#,(match (list (attribute-info #'a)
                            (attribute-info #'sc)
                            (attribute-info #'sc.foo)
                            (attribute-info #'b)
                            (attribute-info #'c)
                            ;
                            (attribute-info #'a      '(pvar))
                            (attribute-info #'sc     '(pvar))
                            (attribute-info #'sc.foo '(pvar))
                            (attribute-info #'b      '(pvar))
                            (attribute-info #'c      '(pvar))
                            ;
                            (attribute-info #'a      '(attr) #f)
                            (attribute-info #'sc     '(attr) #f)
                            (attribute-info #'sc.foo '(attr) #f)
                            (attribute-info #'b      '(attr) #f)
                            (attribute-info #'c      '(attr) #f))
                 [(list (list 'attr _ 1 'a #t)
                        (list 'attr _ 0 'sc #t)
                        (list 'attr _ 0 'sc.foo #t)
                        (list 'attr _ 0 'b #f)
                        (list 'pvar _ 1)
                        ;
                        (list 'pvar _ 1)
                        (list 'pvar _ 0)
                        (list 'pvar _ 0)
                        (list 'pvar _ 0)
                        (list 'pvar _ 1)
                        ;
                        (list 'attr _ 1 'a #t)
                        (list 'attr _ 0 'sc #t)
                        (list 'attr _ 0 'sc.foo #t)
                        (list 'attr _ 0 'b #f)
                        #f)
                  #t]
                 [_
                  #f]))
         tst)])]))