#lang racket/base

(require (for-syntax racket/base
                     racket/private/sc
                     racket/contract
                     racket/syntax)
         syntax/parse
         version-case)

(version-case
 [(version< (version) "6.90.0.24")
  (require (prefix-in - syntax/parse/private/residual))]
 [else
  (require (rename-in (prefix-in - racket/private/template)
                      [-attribute-mapping -make-attribute-mapping]))
  (define-for-syntax (-attribute-mapping-syntax? x)
    ;; attribute-mapping-check is actually false when attribute-mapping-syntax?
    ;; would have been true (thanks rmculpepper !)
    (not (-attribute-mapping-check x)))])

(provide attribute*
         (for-syntax attribute-info)
         define-raw-attribute
         define-raw-syntax-mapping)

(define-syntax (attribute* stx)
  (syntax-case stx ()
    [(_ a)
     (with-disappeared-uses
      (let ()
        (record-disappeared-uses (list #'a))
        (let ([slv (syntax-local-value #'a (λ () #f))])
          (if (syntax-pattern-variable? slv)
              (let* ([valvar (syntax-mapping-valvar slv)]
                     [valvar-slv (syntax-local-value valvar (λ () #f))])
                (if (-attribute-mapping? valvar-slv)
                    (-attribute-mapping-var valvar-slv)
                    valvar))
              (raise-syntax-error
               'attribute*
               "not bound as an attribute or pattern variable"
               stx
               #'a)))))]))

;; The "accept" parameter allows forwards compatibility:
;; if a new sort of syntax pattern variable is added, either it degrades
;; gracefully into one of the accepted kinds, or an error is raised.
;; The client does not have to deal with unknown cases, unless accept is #t.
(begin-for-syntax
  (define/contract (attribute-info a [accept #t] [error? #t])
    (->* {identifier?}
         {(or/c #t (listof symbol?))
          boolean?}
         (or/c #f
               (list/c 'attr
                       identifier? exact-nonnegative-integer? symbol? boolean?)
               (list/c 'pvar
                       identifier? exact-nonnegative-integer?)))
    (define slv (syntax-local-value a (λ () #f)))
           ;; (assert (syntax-pattern-variable? slv))
    (define attr (and slv
                      (syntax-local-value (syntax-mapping-valvar slv)
                                          (λ () #f))))
    (cond
      [(and attr
            (-attribute-mapping? attr)
            (or (eq? #t accept) (and (list? accept) (memq 'attr accept))))
       (list 'attr
             (-attribute-mapping-var attr)
             (-attribute-mapping-depth attr)
             (-attribute-mapping-name attr)
             (-attribute-mapping-syntax? attr))]
      [(and (syntax-pattern-variable? slv)
            (or (eq? #t accept) (and (list? accept) (memq 'pvar accept))))
       (list 'pvar
             (syntax-mapping-valvar slv)
             (syntax-mapping-depth slv))]
      [else
       (when error?
         (raise-syntax-error 'attribute-info
                             "not defined as an attribute or pattern variable"
                             a))
       #f])))

(define-syntax-rule (define-raw-attribute name valvar val depth syntax?)
    (begin
      (define valvar
        val)
      (define-syntax tmp-attr
        (-make-attribute-mapping (quote-syntax valvar)
                                 'name
                                 'depth
                                 'syntax?))
      (define-syntax name
        (make-syntax-mapping 'depth
                             (quote-syntax tmp-attr)))))

  (define-syntax-rule (define-raw-syntax-mapping name valvar val depth)
    (begin
      (define valvar
        val)
      (define-syntax name
        (make-syntax-mapping 'depth (quote-syntax valvar)))))