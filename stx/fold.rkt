#lang racket

(provide fold-syntax
         replace-top-loc
         syntax/top-loc
         quasisyntax/top-loc
         syntax/whole-loc
         quasisyntax/whole-loc)

(define (fold-syntax f stx)
  (let process ([stx stx])
    (cond
      [(syntax? stx)
       (f stx (λ (x)
                (let ([p (process (syntax-e x))])
                  (if (syntax? p)
                      p
                      (datum->syntax stx p stx stx)))))]
      [(pair? stx)
       (cons (process (car stx))
             (process (cdr stx)))]
      [(null? stx)
       stx]
      [(vector? stx)
       (list->vector (map process (vector->list stx)))]
      [(box? stx)
       (box (process (unbox stx)))]
      [(hash? stx)
       (define processed (process (hash->list stx)))
       (cond
         [(hash-equal? stx) (hash processed)]
         [(hash-eqv? stx) (hasheqv processed)]
         [(hash-eq? stx) (hasheq processed)])]
      [(prefab-struct-key stx)
       (apply make-prefab-struct
              (prefab-struct-key stx)
              (map process (vector->list (struct->vector stx))))]
      [else
       stx])))

;; Replaces the syntax/loc for the top of the syntax object, until
;; a part which doesn't belong to old-source is reached.
;; e.g. (with-syntax ([d user-provided-syntax])
;;        (replace-top-loc
;;          #'(a b (c d e))
;;          (syntax-source #'here)
;;          new-loc))
;; will produce a syntax object #'(a b (c (x (y) z) e))
;; where a, b, c, z, e and their surrounding forms have their srcloc set to
;; new-loc, but (x (y) z) will be left intact, if the user-provided-syntax
;; appears in another file.

(define (replace-top-loc stx old-source new-loc)
  (fold-syntax
   (λ (stx rec)
     (if (equal? (syntax-source stx) old-source)
         (datum->syntax stx (syntax-e (rec stx)) new-loc stx)
         stx))
   stx))

;; Use the following function to replace the loc throughout stx
;; instead of stopping the depth-first-search when the syntax-source
;; is not old-source anymore
(define (replace-whole-loc stx old-source new-loc)
  (fold-syntax
   (λ (stx rec)
     (if (equal? (syntax-source stx) old-source)
         (datum->syntax stx (syntax-e (rec stx)) new-loc stx)
         (rec stx)))
   stx))

(define-syntax (syntax/top-loc stx)
  (syntax-case stx ()
    [(self loc template)
     #'(replace-top-loc #'template (syntax-source #'self) loc)]))

(define-syntax (quasisyntax/top-loc stx)
  (syntax-case stx ()
    [(self loc template)
     #'(replace-top-loc #`template (syntax-source #'self) loc)]))

(define-syntax (syntax/whole-loc stx)
  (syntax-case stx ()
    [(self loc template)
     #'(replace-whole-loc #'template (syntax-source #'self) loc)]))

(define-syntax (quasisyntax/whole-loc stx)
  (syntax-case stx ()
    [(self loc template)
     #'(replace-whole-loc #`template (syntax-source #'self) loc)]))