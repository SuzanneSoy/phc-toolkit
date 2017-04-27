#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  ;; intersection types with ∩ were not present in 6.5
  (require "typed-untyped.rkt")
  (if-typed
   (define-syntax (if-typed<6.6 stx)
     (syntax-case stx ()
       [(_ lt ge)
        (if (or (regexp-match #px"^6(\\.[012345](\\..*|)|)$" (version))
                (regexp-match #px"^[123245]\\..*$" (version)))
            #'lt
            #'ge)]))
   (define-syntax-rule (if-typed<6.6 lt ge) ge))
  (define-syntax-rule (skip-typed<6.6 . rest)
    (if-typed<6.6 (begin) (begin . rest)))

  (skip-typed<6.6
   (provide stx-e/c
            stx-e))
  (provide (all-from-out syntax/stx
                         "stx/fold.rkt"
                         "untyped-only/stx.rkt")

           stx-list
           stx-e
           stx-pair

           stx-list/c
           stx-car/c
           stx-cdr/c
           
           syntax-cons-property
           stx-map-nested
           identifier-length
           identifier->string
           (rename-out [identifier->string identifier→string])
           ;stx-map-nested
           
           stx-car
           stx-cdr
           stx-null?
           stx-pair?
           stx-list?
           
           stx-cons
           
           Stx-List?
           Syntax-Pairs-of
           
           stx-drop-last
           stx->list
           
           stx-foldl
           
           stx-assoc
           cdr-stx-assoc
           
           check-duplicate-identifiers

           remove-use-site-scope

           nameof)
  
  (require syntax/stx
           (for-syntax racket/syntax
                       "untyped-only/stx.rkt")
           "typed-untyped.rkt")
  (require-typed/untyped "sequence.rkt")

  (require "stx/fold.rkt"
           "untyped-only/stx.rkt")
  
  ;; match-expanders:
  ;;   stx-list
  ;;   stx-e
  ;;   stx-pair
  (begin
    (define-match-expander stx-list
      (λ (stx)
        (syntax-case stx ()
          [(_ pat ...)
           #'(? syntax?
                (app syntax->list (list pat ...)))])))

    (define-for-syntax stx-e-match-expander
       (λ (stx)
         (syntax-case stx ()
           [(_ pat)
            #'(? syntax?
                 (app syntax-e pat))])))
    
    (if-typed<6.6
     (define-match-expander stx-e
       stx-e-match-expander)
     (define-match-expander stx-e
       stx-e-match-expander
       (make-id+call-transformer #'stx-e-fun)))
    
    (define-match-expander stx-pair
      (λ (stx)
        (syntax-case stx ()
          [(_ pat-car pat-cdr)
           #'(? syntax?
                (app syntax-e (cons pat-car pat-cdr)))]))))
  
  ;; utilities:
  ;;   syntax-cons-property
  ;;   identifier-length
  ;;   identifier->string
  ;;   stx-map-nested
  (begin
    (: syntax-cons-property (∀ (A) (→ (Syntaxof A) Symbol Any (Syntaxof A))))
    (define (syntax-cons-property stx key v)
      (let ([orig (syntax-property stx key)])
        (syntax-property stx key (cons v (or orig '())))))
    
    (: identifier-length (→ Identifier Index))
    (define (identifier-length id) (string-length (identifier->string id)))
    
    (: identifier->string (→ Identifier String))
    (define (identifier->string id) (symbol->string (syntax-e id)))
    
    (: stx-map-nested (∀ (A B) (→ (→ A B)
                                  (Syntaxof (Listof (Syntaxof (Listof A))))
                                  (Listof (Listof B)))))
    (define (stx-map-nested f stx)
      (map (λ ([x : (Syntaxof (Listof A))])
             (map f (syntax-e x)))
           (syntax-e stx))))
  
  ;; accessors:
  ;;   stx-car
  ;;   stx-cdr
  ;;   stx-null?
  ;;   stx-pair?
  (begin
    #|
    (require/typed syntax/stx
                   [stx-car (∀ (A B) (→ (Syntaxof (Pairof A B)) A))]
                   [stx-cdr (∀ (A B) (→ (Syntaxof (Pairof A B)) B))])
    |#
    
    (: stx-car (∀ (A B)
                  (case→ (→ (U (Syntaxof (Pairof A B)) (Pairof A B)) A)
                         ;; TODO: Not typesafe!
                         (→ (U (Syntaxof (Listof A)) (Listof A)) A))))
    (define (stx-car p) (car (if (syntax? p) (syntax-e p) p)))
    
    (: stx-cdr (∀ (A B)
                  (case→ (→ (U (Syntaxof (Pairof A B)) (Pairof A B)) B)
                         ;; TODO: Not typesafe!
                         (→ (U (Syntaxof (Listof A)) (Listof A))
                            (Listof A)))))
    (define (stx-cdr p) (cdr (if (syntax? p) (syntax-e p) p)))

    (: stx-car/c (∀ (Result) (→ (→ Any Result)
                                (→ Any (U #f Result)))))
    (define ((stx-car/c car/c) v)
      (if (syntax? v)
          (if (pair? (syntax-e v))
              (let ([r (car/c (car (syntax-e v)))])
                r)
              #f)
          #f))

    (: stx-cdr/c (∀ (Result) (→ (→ Any Result)
                                (→ Any (U #f Result)))))
    (define ((stx-cdr/c car/c) v)
      (and (if-typed
            ((make-predicate (Syntaxof (Pairof Any Any))) v)
            (and (syntax? v) (pair? (syntax-e v))))
           (car/c (stx-cdr v))))
    
    (: stx-null? (→ Any Boolean : (U (Syntaxof Null) Null)))
    (define (stx-null? v)
      (if-typed
       ((make-predicate (U (Syntaxof Null) Null)) v)
       (or (null? v) (and (syntax? v) (null? (syntax-e v))))))
    
    (: stx-pair? (→ Any Boolean : (U (Pairof Any Any)
                                     (Syntaxof (Pairof Any Any)))))
    (define (stx-pair? v)
      (if-typed
       ((make-predicate (U (Pairof Any Any)
                           (Syntaxof (Pairof Any Any))))
        v)
       (or (pair? v) (and (syntax? v) (pair? (syntax-e v)))))))
  
  ;; constructors:
  ;;   stx-cons
  (begin
    (module m-stx-cons-untyped racket
      (provide stx-cons list->stx list*->stx)
      
      (define (stx-cons a b) #`(#,a . #,b))
      (define (list->stx l) #`#,l)
      (define (list*->stx l*) #`#,l*))
    
    (if-typed
     (module m-stx-cons-typed typed/racket
       (provide stx-cons list->stx list*->stx)
       (require (only-in typed/racket/unsafe unsafe-require/typed))
       (unsafe-require/typed
        (submod ".." m-stx-cons-untyped)
        [stx-cons (∀ (A B)
                     (→ (Syntaxof A)
                        (Syntaxof B)
                        (Syntaxof (Pairof (Syntaxof A) (Syntaxof B)))))]
        [list->stx (∀ (A)
                      (→ (Listof (Syntaxof A))
                         (Syntaxof (Listof (Syntaxof A)))))]
        [list*->stx (∀ (A B)
                       (→ (Rec R (U B (Pairof (Syntaxof A) R)))
                          (Syntaxof (Rec R (U B (Pairof (Syntaxof A) R))))))]))
     (module m-stx-cons-typed racket
       (provide stx-cons list->stx list*->stx)
       (require (submod ".." m-stx-cons-untyped))))
    
    (require 'm-stx-cons-typed))

  ;; stx-drop-last
  (begin
    (: drop-last (∀ (A) (→ (Listof A) (Listof A))))
    (define (drop-last l)
      (if (and (pair? l) (pair? (cdr l)))
          (cons (car l) (drop-last (cdr l)))
          '()))
    
    (define-type (Stx-List? A)
      (U Null
         (Pairof A (Stx-List? A))
         (Syntaxof Null)
         (Syntaxof (Pairof A (Stx-List? A)))))

    (: stx-list? (→ Any Boolean : (Stx-List? Any)))
    (define (stx-list? v)
      (if-typed ((make-predicate (Stx-List? Any)) v)
                (or (null? v)
                    (and (pair? v) (stx-list? (cdr v)))
                    (and (syntax? v) (null? (syntax-e v)))
                    (and (syntax? v) (stx-list? (cdr (syntax-e v)))))))
    
    (: stx-list/c (∀ (Result) (→ (→ (Listof Any) Result)
                                 (→ Any (U #f Result)))))
    (define ((stx-list/c l/c) v)
      (and (stx-list? v)
           (l/c (stx->list v))))
    
    (define-type (Syntax-Pairs-of A)
      (U (Syntaxof Null)
         (Syntaxof (Pairof A (Syntax-Pairs-of A)))))

    (: stx->list (∀ (A) (→ (Stx-List? A) (Listof A))))
    (define (stx->list l)
      (cond [(null? l)
             '()]
            [(pair? l)
             (cons (car l) (stx->list (cdr l)))]
            [else
             (stx->list (syntax-e l))]))
    
    (: stx-drop-last
       (∀ (A) (→ (Stx-List? (Syntaxof A)) (Syntaxof (Listof (Syntaxof A))))))
    (define (stx-drop-last l)
      (list->stx (drop-last (stx->list l))))

    ;; stx-e-fun is used as the fallback for the stx-e match-expander
    (define-type SexpofAny1 (U Boolean
                               Complex
                               Char
                               Null
                               Symbol
                               String
                               Keyword
                               (Pairof Any Any)
                               VectorTop
                               BoxTop))

    (skip-typed<6.6
     (: stx-e/c (∀ (Result) (→ (→ Any Result)
                               (→ Any (U #f Result)))))
     (define ((stx-e/c e/c) v)
       (and (if-typed ((make-predicate (U (Syntaxof Any) SexpofAny1)) v)
                      #t) ;; The untyped stx-e-fun is more permissive
            (e/c (stx-e-fun v))))

     (: stx-e-fun (∀ (A) (case→ (→ (U (Syntaxof A) (∩ A SexpofAny1))
                                   A))))
     (define (stx-e-fun v)
       (if (syntax? v)
           (syntax-e v)
           v)))
    #|
      #;(cond [(null? l)
             #'()]
            [(pair? l)
             (cond [(null? (cdr l))
                    #'()]
                   [(pair? (cdr l))
                    ]
                   [else
             (let* ([res (stx-drop-last (cdr l))]
                    [e (syntax-e res)])
               (if (null? e)
                   (stx-cons (car l) #'())
                   (stx-cons (car l) res)))]
            [else
             (stx-drop-last (syntax-e l))])
      
      #;(if (if-typed ((make-predicate (Syntaxof Any)) l) (syntax? l))
          (stx-drop-last (syntax-e l))
          (if (null? l)
              #'()
              (stx-cons (car l)
                        (stx-drop-last (cdr l)))))))
      |#)
  
  ;; stx-foldl
  (begin
    (: stx-foldl
       (∀ (E F G Acc)
          (case→ (→ (→ E Acc Acc)
                    Acc
                    (U (Syntaxof (Listof E)) (Listof E))
                    Acc)
                 (→ (→ E F Acc Acc)
                    Acc
                    (U (Syntaxof (Listof E)) (Listof E))
                    (U (Syntaxof (Listof F)) (Listof F))
                    Acc)
                 (→ (→ E F G Acc Acc)
                    Acc
                    (U (Syntaxof (Listof E)) (Listof E))
                    (U (Syntaxof (Listof F)) (Listof F))
                    (U (Syntaxof (Listof G)) (Listof G))
                    Acc))))
    (define stx-foldl
      (case-lambda
        [(f acc l)
         (if (stx-null? l)
             acc
             (stx-foldl f (f (stx-car l) acc) (stx-cdr l)))]
        [(f acc l l2)
         (if (or (stx-null? l) (stx-null? l2))
             acc
             (stx-foldl f
                        (f (stx-car l) (stx-car l2) acc)
                        (stx-cdr l)
                        (stx-cdr l2)))]
        [(f acc l l2 l3)
         (if (or (stx-null? l) (stx-null? l2) (stx-null? l3))
             acc
             (stx-foldl f
                        (f (stx-car l) (stx-car l2) (stx-car l3) acc)
                        (stx-cdr l)
                        (stx-cdr l2)
                        (stx-cdr l3)))])))
  
  ;; stx-assoc
  ;; cdr-stx-assoc
  (begin
    (: stx-assoc (∀ (T) (case→
                         (→ Identifier
                            (U (Syntaxof (Listof (Syntaxof (Pairof Identifier
                                                                   T))))
                               (Listof (Syntaxof (Pairof Identifier T))))
                            (U (Syntaxof (Pairof Identifier T)) #f))
                         (→ Identifier
                            (Listof (Pairof Identifier T))
                            (U (Pairof Identifier T) #f)))))
    (define (stx-assoc id alist)
      (let* ([e-alist (if (syntax? alist)
                          (syntax->list alist)
                          alist)]
             [e-e-alist (cond
                          [(null? e-alist) '()]
                          [(syntax? (car e-alist))
                           (map (λ ([x : (Syntaxof (Pairof Identifier T))])
                                  (cons (stx-car x) x))
                                e-alist)]
                          [else
                           (map (λ ([x : (Pairof Identifier T)])
                                  (cons (car x) x))
                                e-alist)])]
             [result (assoc id e-e-alist free-identifier=?)])
        (if result (cdr result) #f)))
    
    (: cdr-stx-assoc
       (∀ (T) (case→ (→ Identifier
                        (U (Syntaxof (Listof (Syntaxof (Pairof Identifier T))))
                           (Listof (Syntaxof (Pairof Identifier T)))
                           (Listof (Pairof Identifier T)))
                        (U T #f)))))
    (define (cdr-stx-assoc id alist)
      (if (null? alist)
          #f
          ;; The typechecker is not precise enough, and the code below does not
          ;; work if we factorize it:
          ;; (if (and (list? alist) (syntax? (car alist))) … …)
          (if (list? alist)
              (if (syntax? (car alist))
                  (let ((res (stx-assoc id alist)))
                    (if res (stx-cdr res) #f))
                  (let ((res (stx-assoc id alist)))
                    (if res (cdr res) #f)))
              (let ((res (stx-assoc id alist)))
                (if res (stx-cdr res) #f))))))
  
  ;; check-duplicate-identifiers
  (begin
    (: check-duplicate-identifiers (→ (Syntaxof (Listof (Syntaxof Symbol)))
                                      Boolean))
    (define (check-duplicate-identifiers ids)
      (if (check-duplicate-identifier (my-in-syntax ids)) #t #f)))

  ;; remove-use-site-scope
  (begin
    (define #:∀ (A) (remove-use-site-scope [stx : (Syntaxof A)])
      (define bd
        (syntax-local-identifier-as-binding (syntax-local-introduce #'here)))
      (define delta
        (make-syntax-delta-introducer (syntax-local-introduce #'here) bd))
      (delta stx 'remove)))

  ;; nameof
  (begin
    ;; TODO: use the proper way to introduce arrows if possible.
    (define-syntax (nameof stx)
      (syntax-case stx ()
        [(_ x)
         (record-disappeared-uses (list #'x))
         #''x])))
  
  #|
  (define (raise-multi-syntax-error name message exprs)
    (let ([e (exn:fail:syntax "message"
                              (current-continuation-marks)
                              (list #'aaa #'bbb))])
      ((error-display-handler) (exn-message e) e)))
  |#)