#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (provide hash-set**
           ;string-set!
           ;string-copy!
           ;string-fill!
           with-output-file
           or?)
  
  (require (for-syntax syntax/parse syntax/parse/experimental/template))
  
  ;; hash-set**: hash-set a list of K V pairs.
  (begin
    (: hash-set** (∀ (K V)
                     (→ (HashTable K V) (Listof (Pairof K V)) (HashTable K V))))
    (define (hash-set** h l)
      (if (null? l)
          h
          (hash-set** (hash-set h (caar l) (cdar l)) (cdr l)))))
  
  ;; Disable string mutation
  (begin
    (define-syntax (string-set! stx)
      (raise-syntax-error 'string-set! "Do not mutate strings." stx))
    (define-syntax (string-copy! stx)
      (raise-syntax-error 'string-copy! "Do not mutate strings." stx))
    (define-syntax (string-fill! stx)
      (raise-syntax-error 'string-fill! "Do not mutate strings." stx)))
  
  ;; with-output-file
  (begin
    #|
    (define-syntax (with-output-file stx)
      (syntax-parse stx
        [(_ filename:expr (~optional (~seq #:mode mode:expr))
            (~optional (~seq #:exists exists:expr))
            body ...)
         (template (with-output-to-file filename
                     (λ () body ...)
                     (?? (?@ #:mode mode))
                     (?? (?@ #:exists exists))))]))
    |#
    
    (define-syntax (with-output-file stx)
      (syntax-parse stx
        [(_ [var:id filename:expr]
            (~optional (~seq #:mode mode:expr))
            (~optional (~seq #:exists exists:expr))
            body ...)
         (template (call-with-output-file filename
                     (λ (var) body ...)
                     (?? (?@ #:mode mode))
                     (?? (?@ #:exists exists))))])))
  
  #;(: or? (∀ (A B) (case→ (→ (→ A A))
                           (→ (→ A B) (→ A B) * (→ A B)))))
  #;(define or?
      (case-lambda
        [() (λ (a)
              a)]
        [(f . f*) (λ (a)
                    (let ([b (f a)])
                      (if (or b (null? f*))
                          b
                          ((apply or? f*) a))))]))
  
  (: or? (∀ (A) (→ (→ A Boolean) * (→ A (U A #f)))))
  (define (or? . f*)
    (if (null? f*)
        (λ (a) a)
        (λ (a)
          (if ((car f*) a)
              a
              ((apply (inst or? A) (cdr f*)) a))))))