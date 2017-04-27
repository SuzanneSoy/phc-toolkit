#lang racket

(require "../typed-untyped.rkt")
(define-typed/untyped-test-module
  (require-typed/untyped phc-toolkit/ids)
  (require-typed/untyped phc-toolkit/typed-rackunit)
  (require (for-syntax racket/syntax
                       phc-toolkit/untyped/ids))
    
  (check-equal?: (format-ids #'a "~a-~a" #'() #'())
                 '())
    
  (check-equal?: (map syntax->datum
                      (format-ids #'a "~a-~a" #'(x1 x2 x3) #'(a b c)))
                 '(x1-a x2-b x3-c))
    
  ;; Since the presence of "Syntax" in the parameters list makes format-ids
  ;; require a chaperone contract instead of a flat contract, we can't run the
  ;; two tests below directly, we would need to require the untyped version of
  ;; this file, which causes a cycle in loading.
    
  (define-syntax (test1 stx)
    (syntax-case stx ()
      [(_ (let1 d1) x y)
       (begin
         (define/with-syntax (foo-x foo-y)
           (format-ids (λ (xy)
                         (if (string=? (symbol->string (syntax->datum xy))
                                       "b")
                             stx
                             #'()))
                       "foo-~a"
                       #'(x y)))
         #'(let1 d1 (let ((foo-b 2) (foo-c 'b)) (cons foo-x foo-y))))]))
    
  (check-equal?: (test1 (let ((foo-b 1) (foo-c 'a))) b c)
                 '(1 . b))
    
  (define-syntax (fubar stx)
    (define/with-syntax (v1 ...) #'(1 2 3))
    (define/with-syntax (v2 ...) #'('a 'b 'c))
    ;; the resulting ab and ab should be distinct identifiers:
    (define/with-syntax (id1 ...) (format-temp-ids "~a" #'(ab cd ab)))
    (define/with-syntax (id2 ...) (format-temp-ids "~a" #'(ab cd ab)))
    #'(let ([id1 v1] ...)
        (let ([id2 v2] ...)
          (list (cons id1 id2) ...))))
    
  (check-equal?: (fubar) '((1 . a) (2 . b) (3 . c)))

  (define-syntax (test-concise stx)
    (syntax-case stx ()
      [(_ a ...)
       (let ()
         (define-temp-ids #:concise "~a!" (a ...))
         #''(a! ...))]))
  (check-equal? (test-concise one "two" 3)
                '(one! two! 3!)))