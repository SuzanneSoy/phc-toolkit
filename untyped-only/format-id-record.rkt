#lang racket/base

(module m racket/base
  (require racket/contract
           racket/syntax
           racket/stxparam
           syntax/stx
           syntax/parse
           (submod "../syntax-parse.rkt" untyped)
           (for-syntax racket/base))

  (provide sub-range-binder/c
           current-recorded-sub-range-binders
           maybe-record-sub-range-binders!
           record-sub-range-binders!
           with-sub-range-binders
           with-arrows
           syntax-parser-with-arrows
           format-id/record)

  (define sub-range-binder/c
    (or/c (vector/c syntax?
                    exact-nonnegative-integer? exact-nonnegative-integer?
                    (real-in 0 1) (real-in 0 1)
                    syntax?
                    exact-nonnegative-integer? exact-nonnegative-integer?
                    (real-in 0 1) (real-in 0 1))
          (vector/c syntax?
                    exact-nonnegative-integer? exact-nonnegative-integer?
                    syntax?
                    exact-nonnegative-integer? exact-nonnegative-integer?)))
  
  (define/contract current-recorded-sub-range-binders
    (parameter/c (or/c (listof sub-range-binder/c) false/c))
    (make-parameter #f))

  ;; TODO: should use a parameter in addition to the error? argument.
  (define/contract ((record-sub-range-binders/check! error?) sub-range-binders)
    (-> boolean?
        (-> (or/c sub-range-binder/c (listof sub-range-binder/c))
            void?))
    (if (current-recorded-sub-range-binders)
        (if (list? sub-range-binders)
            (current-recorded-sub-range-binders
             (append sub-range-binders (current-recorded-sub-range-binders)))
            (current-recorded-sub-range-binders
             (cons sub-range-binders (current-recorded-sub-range-binders))))
        (when error?
          (error
           (format
            (string-append "record-sub-range-binders should be used within the"
                           " dynamic extent of with-sub-range-binders,"
                           " with-arrows or a similar form. Arguments were: ~a")
            sub-range-binders)))))

  (define/contract maybe-record-sub-range-binders!
    (-> (or/c sub-range-binder/c (listof sub-range-binder/c))
        void?)
    (record-sub-range-binders/check! #f))

  (define/contract record-sub-range-binders!
    (-> (or/c sub-range-binder/c (listof sub-range-binder/c))
        void?)
    (record-sub-range-binders/check! #t))
  
  (define-syntax-rule (with-sub-range-binders . body)
    (parameterize ([current-recorded-sub-range-binders '()])
      (syntax-property (let () . body)
                       'sub-range-binders
                       (current-recorded-sub-range-binders))))

  (define-syntax-rule (with-arrows . body)
    (with-disappeared-uses
     (with-sub-range-binders
      . body)))

  (define-syntax-rule (syntax-parser-with-arrows . opts+clauses)
    (λ (stx2)
      (with-disappeared-uses
       (with-sub-range-binders
        (syntax-parameterize ([stx (make-rename-transformer #'stx2)])
          ((syntax-parser . opts+clauses) stx2))))))

  (define (identifier-length id)
    (string-length (symbol->string (syntax-e id))))

  (define (formatted-length v)
    (identifier-length (format-id #f "~a" v)))

  (define (format-length fmt)
    (identifier-length (format-id #f fmt)))
  
  (define/contract (format-id/record lctx
                                     fmt
                                     #:source [src #f]
                                     #:props [props #f]
                                     . vs)
    ;; TODO: use check-restricted-format-string from racket/syntax.rkt
    (->* {(or/c syntax? #f)
          (or/c string? (syntax/c string?))}
         {#:source (or/c syntax? #f)
          #:props (or/c syntax? #f)}
         #:rest (listof (or/c string? symbol? keyword? char? number?
                              (syntax/c string?)
                              identifier?
                              (syntax/c keyword?)
                              (syntax/c char?)
                              (syntax/c number?)))
         identifier?)

    (define e-vs (stx-map (λ (v) (if (and (syntax? v) (not (identifier? v)))
                                     (syntax-e v)
                                     v))
                          vs))
    (define str-fmt (if (syntax? fmt) (syntax-e fmt) fmt))
    (define whole (apply format-id lctx str-fmt e-vs
                         #:source src
                         #:props props))
    (define split-fmt (regexp-split #px"~[aA]" str-fmt))

    ;; sub-range-binder for the first static part of the format
    (when (syntax? fmt)
      (record-sub-range-binders!
       (vector (syntax-local-introduce whole)
               0 (format-length (car split-fmt))
               fmt
               1 (string-length (car split-fmt))))) ;; +1 for #\"
    
    (for/fold ([input-len (+ 1 (string-length (car split-fmt)))] ;; +1 for #\"
               [output-len (string-length (car split-fmt))])
              ([v (in-list vs)]
               [e-v (in-list e-vs)]
               [fmt-part (cdr split-fmt)])
      (define v-len (formatted-length e-v))
      (define fmt-output-len (format-length fmt-part))
      (define fmt-input-len (string-length fmt-part))
      ;; sub-range binder for the ~a
      (record-sub-range-binders!
       (vector (syntax-local-introduce whole)
               output-len v-len
               v
               0 v-len))
      ;; sub-range-binder for the static part of the format
      (when (syntax? fmt)
        (record-sub-range-binders!
         (vector (syntax-local-introduce whole)
                 (+ output-len v-len) fmt-output-len
                 fmt
                 (+ input-len 2) fmt-input-len))) ;; +2 for the "~a"
      ;; loop with format-len and output-len =
      (values (+ input-len 2 fmt-input-len) ;; +2 for the "~a"
              (+ output-len v-len fmt-output-len)))
    whole))

(module m2 racket/base
  (require (for-syntax (submod ".." m)
                       phc-toolkit/untyped/aliases
                       syntax/parse
                       racket/function
                       syntax/stx
                       (only-in (submod "../stx.rkt" untyped)
                                remove-use-site-scope)))
  (provide inject-sub-range-formats)

  (require racket/splicing
           (for-syntax racket/base))

  (define-syntax (inject-sub-range-formats stx)
    ;; for some reason, callijng remove-use-site-scope on the whole stx object
    ;; does not work.
    (define clean-stx (remove-use-site-scope stx))
    (syntax-case stx (); parser
      [(_ ([lctx fmt vs …] …) . body);(_ ([-lctx -fmt -vs …] …) . -body)
       ;#:with (lctx …)   (stx-map remove-use-site-scope #'(-lctx …))
       ;#:with (fmt …)    (stx-map remove-use-site-scope #'(-fmt …))
       ;#:with ((vs …) …) (stx-map (curry stx-map remove-use-site-scope)
       ;                           #'((-vs …) …))
       ;#:with body  (remove-use-site-scope #'-body)
       (remove-use-site-scope
        #'(splicing-let-syntax
              ([tmp (λ _
                      (with-sub-range-binders
                       (format-id/record lctx fmt vs …)
                       …
                       (remove-use-site-scope
                        (syntax-local-introduce
                         (quote-syntax (begin . body))))))])
            (tmp)))])))

(module m3 racket/base
  (require racket/require-syntax
           (for-syntax racket/base
                       racket/list)
           racket/stxparam
           racket/syntax)
  
  (define-require-syntax (for-many stx)
    (syntax-case stx ()
      [(_ require-spec ...)
       #`(combine-in #,@(map (λ (n) #`(for-meta #,n require-spec ...))
                             (range -16 17)))]))
  ;; If the level 1 macro using with-format-ids/inject-binders places
  ;; inject-sub-range-binders ... in a level 0 form, then 'm2 is needed
  ;; for-template. However, if inject-sub-range-binders ... appears in
  ;; a level 1 form, then 'm2 is needed for-meta 0. If
  ;; inject-sub-range-binders ... appears in a level 2 form, then 'm2 is
  ;; needed for-meta 1, etc. We therefore require it many times, for all
  ;; meta-levels from -16 to 16, which should be plenty enough for all
  ;; practical purposes.
  (require (for-template (for-many (submod ".." m2))))
  
  (provide with-format-ids/inject-binders
           inject-sub-range-binders)

  (define-syntax (inject-sub-range-binders-init stx)
    (raise-syntax-error 'inject-sub-range-binders
                        "must be used inside with-format-ids/inject"
                        stx))
  (define-rename-transformer-parameter inject-sub-range-binders
    (make-rename-transformer #'inject-sub-range-binders-init))

  (define-syntax-rule
      (with-format-ids/inject-binders ([id lctx fmt vs ...] ...) . body)
    (with-syntax
        ([(fmts (... ...))
          #'(inject-sub-range-formats ([lctx fmt vs ...] ...))])
      (syntax-parameterize
          ([inject-sub-range-binders (make-rename-transformer #'fmts)])
        (with-syntax ([id (format-id lctx fmt vs ...)] ...)
          (let ()
            . body))))))

(require 'm
         (for-template 'm2)
         'm3)

(provide (all-from-out 'm)
         (for-template inject-sub-range-formats)
         (all-from-out 'm3))