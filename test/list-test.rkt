#lang typed/racket
(require "../typed-untyped.rkt")
(define-typed/untyped-test-module
  (require-typed/untyped phc-toolkit/list
                         phc-toolkit/typed-rackunit)
  
  (check-equal?: (indexof 'c '(c)) 0)
  (check-equal?: (indexof 'c '(c a b c d a b c d)) 0)
  (check-equal?: (indexof 'c '(a b c d a b c d)) 2)
  (check-equal?: (indexof 'x '()) #f)
  (check-equal?: (indexof 'x '(c)) #f)
  (check-equal?: (indexof 'x '(c a b c d a b c d)) #f)
  (check-equal?: (indexof 'x '(a b c d a b c d)) #f)

  (define-syntax (skip<=6.6 stx)
    (syntax-case stx ()
      [(_ . rest)
       (if (or (regexp-match #px"^6(\\.[012345](\\..*|)|)$" (version))
               (regexp-match #px"^6.6$" (version))
               (regexp-match #px"^[123245]\\..*$" (version)))
           #'(begin)
           #'(begin . rest))]))
  
  ;; replace-first
  (skip<=6.6
    (check-equal?: (replace-first 'c 'r '(c)) '(r))
    (check-equal?: (replace-first 'c 'r '(c a b c d a b c d))
                   '(r a b c d a b c d))
    (check-equal?: (replace-first 'c 'r '(a b c d a b c d)) '(a b r d a b c d))
    (check-equal?: (replace-first 'x 'r '()) '())
    (check-equal?: (replace-first 'x 'r '(c)) '(c))
    (check-equal?: (replace-first 'x 'r '(c a b c d a b c d))
                   '(c a b c d a b c d))
    (check-equal?: (replace-first 'x 'r '(a b c d a b c d)) '(a b c d a b c d))
    
    (struct s ([a : Number]) #:transparent)
    (check-equal?: (replace-first (s 2) 'r (list (s 3) (s 2) (s 1) (s 2)))
                   (list (s 3) (s 2) (s 1) (s 2)))
    (check-equal?: (replace-first (s 2)
                                  'r
                                  (list (s 3) (s 2) (s 1) (s 2))
                                  equal?)
                   (list (s 3) 'r (s 1) (s 2)))
    
    (define-type (Test-List3-Maybe Start Mid End)
      (U (Pairof Start (Test-List3-Maybe Start Mid End))
         Null
         (Pairof Mid (Listof End))))
    
    (check-equal?: (replace-first (s 3) 'r (list (s 4) (s 3) (s 2) (s 1) (s 3)))
                   : (Test-List3-Maybe s 'r s)
                   (list (s 4) (s 3) (s 2) (s 1) (s 3)))
    
    (check-equal?: (replace-first (s 3) 'r (list (s 4) (s 3) (s 2) (s 1) (s 3)))
                   : (Rec R (U (Pairof s R)
                               Null
                               (Pairof 'r (Listof s))))
                   (list (s 4) (s 3) (s 2) (s 1) (s 3)))
    
    (check-equal?: (replace-first (s 3) 'r (list (s 4) (s 3) (s 2) (s 1) (s 3)))
                   : (List3-Maybe s 'r s)
                   (list (s 4) (s 3) (s 2) (s 1) (s 3)))
    
    (check-equal?: (replace-first 'r (list 'a 'b 'c 'a 'b 'c)
                                  (λ (x) (eq? x 'c)))
                   : (List3-Maybe (U 'a 'b) 'r (U 'a 'b 'c))
                   (list 'a 'b 'r 'a 'b 'c))
    
    ;; TR is not strong enough yet to infer the type to use, but at least we can
    ;; prove the result has the desired type without using casts:
    (check-equal?: ((inst (ann replace-first
                               (∀ (A B1 B2 C D)
                                  (→ C
                                     (Listof* A (U Null (Pairof B2 D)))
                                     (→ (U A B2) Any :
                                        #:+ (! A) ;; ∴ (and (! A) B2)
                                        #:- (! B2))
                                     (Listof* A (U Null (Pairof C D))))))
                          (U 'a 'b) Nothing 'c 'r (Listof (U 'd 'e)))
                    'r
                    (ann (list 'a 'b 'c 'd 'e)
                         (List3-Maybe (U 'a 'b) 'c (U 'd 'e)))
                    (λ (x) (eq? x 'c)))
                   : (List3-Maybe (U 'a 'b) 'r (U 'd 'e))
                   (list 'a 'b 'r 'd 'e))
    
    ;; TR is not strong enough yet to infer the type to use, but at least we can
    ;; prove the result has the desired type without using casts:
    (check-equal?: ((inst (ann replace-first
                               (∀ (A B1 B2 C D)
                                  (→ C
                                     (Listof* A (Pairof B2 D))
                                     (→ (U A B2) Any :
                                        #:+ (! A) ;; ∴ (and (! A) B2)
                                        #:- (! B2))
                                     (Listof* A (Pairof C D)))))
                          (U 'a 'b) Nothing 'c 'r (Listof (U 'd 'e)))
                    'r
                    (ann (list 'a 'b 'c 'd 'e)
                         (List3 (U 'a 'b) 'c (U 'd 'e)))
                    (λ (x) (eq? x 'c)))
                   : (List3 (U 'a 'b) 'r (U 'd 'e))
                   (list 'a 'b 'r 'd 'e))
    
    ;; TR is not strong enough yet to infer the type to use, but at least we can
    ;; prove the result has the desired type without using casts:
    (check-equal?: ((inst (ann replace-first
                               (∀ (A B1 B2 C D)
                                  (→ C
                                     (Listof* A (Pairof B2 D))
                                     (→ (U A B2) Any :
                                        #:+ (! A) ;; ∴ (and (! A) B2)
                                        #:- (! B2))
                                     (Listof* A (Pairof C D)))))
                          (U 'a 'b) Nothing 'c 'r (List))
                    'r
                    (ann (list 'a 'b 'c)
                         (Listof* (U 'a 'b) (List 'c)))
                    (λ (x) (eq? x 'c)))
                   : (Listof* (U 'a 'b) (List 'r))
                   (list 'a 'b 'r)))
  
  ;; map+fold
  (begin
    (check-equal?: (let-values ([(l a) (map+fold (λ ([e : Number] [a : Number])
                                                   (values (add1 e)
                                                           (+ a e)))
                                                 0
                                                 '(1 2 3 4 5))])
                     (list l a))
                   '((2 3 4 5 6) 15))
    
    (check-equal?: (let-values ([(l a) (map+fold (λ ([e : Number] [a : Number])
                                                   (values 1 2))
                                                 7
                                                 '())])
                     (list l a))
                   '(() 7))
    
    (check-equal?: (let-values ([(l a) (map+fold (λ ([e : Number] [a : Number])
                                                   (values 1 2))
                                                 7
                                                 '(3))])
                     (list l a))
                   '((1) 2))))