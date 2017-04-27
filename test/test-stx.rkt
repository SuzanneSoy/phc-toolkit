#lang typed/racket
(require "../typed-untyped.rkt")
(define-typed/untyped-test-module
  (require-typed/untyped "../typed-rackunit.rkt"
                         "../typed-rackunit-extensions.rkt"
                         "../stx.rkt")

  (check-ann #'() (Stx-List? (Syntaxof Number)))
  (check-ann #'(1) (Stx-List? (Syntaxof Number)))
  (check-ann #'(1 2 3) (Stx-List? (Syntaxof Number)))
  (check-ann #'(1 2 . ()) (Stx-List? (Syntaxof Number)))
  (check-ann #'(1 . (2 . (3 . ()))) (Stx-List? (Syntaxof Number)))
  (check-ann #'(1 . (2 3 . ())) (Stx-List? (Syntaxof Number)))
  (check-ann #'(1 2 . (3 4 . (5))) (Stx-List? (Syntaxof Number)))

  (test-begin
   (check-equal?: (match #'(1 2 3)
                    [(stx-list a b c) (list (syntax-e c)
                                            (syntax-e b)
                                            (syntax-e a))])
                  '(3 2 1))
      
   (check-equal?: (match #'(1 2 3)
                    [(stx-list a ...) (map (inst syntax-e Positive-Byte) a)])
                  '(1 2 3))
      
   #;(check-equal?: (match #`(1 . (2 3))
                      [(stx-list a b c) (list (syntax-e c)
                                              (syntax-e b)
                                              (syntax-e a))])
                    '(3 2 1)))

  (test-begin
   (check-equal? (match #'x [(stx-e s) s]) 'x)
   (check-equal? (match #'(x . y) [(stx-e (cons a b)) (cons (syntax-e b)
                                                            (syntax-e a))])
                 '(y . x)))

  (test-begin
   (check-equal? (match #'(x . y) [(stx-pair a b) (cons (syntax-e b)
                                                        (syntax-e a))])
                 '(y . x))
   (check-equal? (match #'(x y z) [(stx-pair a b) (cons (map syntax->datum b)
                                                        (syntax->datum a))])
                 '((y z) . x)))

  (test-begin
   (check-equal? (stx-null? #f) #f)
   (check-equal? (stx-null? 'a) #f)
   (check-equal? (stx-null? '()) #t)
   (check-equal? (stx-null? #'()) #t)
   (check-equal? (stx-null? #''()) #f)
   (check-equal? (stx-null? #'a) #f))

  (test-begin
   (check-equal? (syntax->datum
                  (ann (stx-cons #'a #'(b c))
                       (Syntaxof (Pairof (Syntaxof 'a)
                                         (Syntaxof (List (Syntaxof 'b)
                                                         (Syntaxof 'c)))))))
                 '(a b c))
      
   (check-equal? (syntax->datum
                  (ann (stx-cons #'1 (ann #'2 (Syntaxof 2)))
                       (Syntaxof (Pairof (Syntaxof 1)
                                         (Syntaxof 2)))))
                 '(1 . 2)))

  (test-begin
   (let ((y 3))
     (check-equal? (nameof y) 'y)))

  (define-syntax (skip<6.6 stx)
    (syntax-case stx ()
      [(_ . rest)
       (if (or (regexp-match #px"^6(\\.[012345](\\..*|)|)$" (version))
               (regexp-match #px"^[123245]\\..*$" (version)))
           #'(begin)
           #'(begin . rest))]))
  (skip<6.6
   (test-begin
    (check-ann (stx-e #'(a . b))
               (Pairof (Syntaxof 'a) (Syntaxof 'b)))

    (check-ann (stx-e `(,#'a . ,#'b))
               (Pairof (Syntaxof 'a) (Syntaxof 'b)))
   
    (check-ann (stx-e '(a . b))
               (Pairof 'a 'b))

    (check-ann (stx-e #'(a b . (c d)))
               (List* (Syntaxof 'a) (Syntaxof 'b)
                      (Syntaxof (List (Syntaxof 'c)
                                      (Syntaxof 'd)))))

    (check-ann (stx-e `(,#'a ,#'b . ,#'(c d)))
               (List* (Syntaxof 'a) (Syntaxof 'b)
                      (Syntaxof (List (Syntaxof 'c)
                                      (Syntaxof 'd))))))))