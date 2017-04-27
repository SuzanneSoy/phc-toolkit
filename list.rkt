#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (define-syntax (skip<=6.6 stx)
    (syntax-case stx ()
      [(_ . rest)
       (if (or (regexp-match #px"^6(\\.[012345](\\..*|)|)$" (version))
               (regexp-match #px"^6.6$" (version))
               (regexp-match #px"^[123245]\\..*$" (version)))
           #'(begin)
           #'(begin . rest))]))

  (skip<=6.6
   (provide replace-first))
  
  (provide indexof
           map+fold
           AListof
           List3-Maybe
           List3
           Listof*)
  
  (define-type (AListof K V) (Listof (Pairof K V)))
  (define-match-expander alistof
    (λ (stx)
      (syntax-case stx ()
        [(keys-pat vals-pat)
         #'(list (cons keys-pat vals-pat) …)])))
  
  (: indexof (∀ (A B) (->* [A (Listof B)] [(→ A B Any)] (U #f Integer))))
  (define (indexof elt lst [compare equal?])
    (let rec ([lst lst] [index 0])
      (if (null? lst)
          #f
          (if (compare elt (car lst))
              index
              (rec (cdr lst) (+ index 1))))))
  
  (define-type (List3-Maybe Start Mid End)
    (Listof* Start
             (U Null
                (Pairof Mid (Listof End)))))
  
  (define-type (List3 Start Mid End)
    (Listof* Start
             (Pairof Mid (Listof End))))
  
  (define-type (Listof* Start End)
    (Rec R (U (Pairof Start R)
              End)))

  (skip<=6.6
   (: replace-first (∀ (A B1 B2 C D)
                       (case→
                        (→ C
                           (Listof (U A B1))
                           (→ (U A B1) Any : #:+ B1 #:- (! B1))
                           (List3-Maybe A C (U A B1)))
                        (→ C
                           (Listof* A (U Null (Pairof B2 D)))
                           (→ (U A B2) Any : #:+ (! A) ;; ∴ (and (! A) B2)
                              #:- (! B2))
                           (Listof* A (U Null (Pairof C D))))
                        (→ C
                           (Listof* A (Pairof B2 D))
                           (→ (U A B2) Any : #:+ (! A) ;; ∴ (and (! A) B2)
                              #:- (! B2))
                           (Listof* A (Pairof C D)))
                        (→ C
                           (Listof A)
                           (→ (U A B1) Any)
                           (List3-Maybe A C (U A B1)))
                        (→ A
                           C
                           (Listof A)
                           (List3-Maybe A C (U A B1)))
                        (→ A
                           C
                           (Listof A)
                           (→ A (U A B1) Any)
                           (List3-Maybe A C (U A B1))))))
   (define (replace-first a1 a2 a3 [a4 eq?])
     (if (list? a3)
         (replace-first a2 a3 (λ ([x : (U A B1)]) (a4 a1 x)))
         (let ([to a1]
               [pred? a3])
           (let rec ([l a2])
             (if (null? l)
                 '()
                 (if (pred? (car l))
                     (cons to (cdr l))
                     (cons (car l)
                           (rec (cdr l))))))))))
  
  (: map+fold (∀ (E R A) (→ (→ E A (values R A)) A (Listof E)
                            (Values (Listof R) A))))
  (define (map+fold f init-acc lst)
    (if (null? lst)
        (values '() init-acc)
        (let*-values ([(item new-acc) (f (car lst) init-acc)]
                      [(new-lst last-acc) (map+fold f new-acc (cdr lst))])
          (values (cons item new-lst)
                  last-acc)))))