#lang typed/racket
(require "typed-untyped.rkt")

(module m-stx-identifier racket
  (require racket/stxparam)
  
  (provide stx)
  
  (define-syntax-parameter stx
    (lambda (call-stx)
      (raise-syntax-error
       'stx
       (string-append "Can only be used in define-syntax/parse, λ/syntax-parse"
                      " or other similar forms")
       call-stx))))

(define-typed/untyped-modules #:no-test
  (provide stx
           define-and-for-syntax
           define-syntax/parse
           define-syntax/case
           ;define-for-syntax/parse-args
           define-for-syntax/case-args
           λ/syntax-parse
           λ/syntax-case
           define/case-args
           λstx
           ~maybe
           ~maybe*
           ~optkw
           ~oncekw
           ~optkw…
           ~oncekw…
           ~kw
           ~lit
           ~with
           ~attr
           ~or-bug
           ~rx-id
           (rename-out [~or-bug ~either])
           define-simple-macro
           ;template/loc
           ;quasitemplate/loc
           template/debug
           quasitemplate/debug
           meta-eval
           define/with-parse
           identity-macro
           name-or-curry
           (all-from-out "untyped-only/syntax-parse.rkt"))
  
  (begin-for-syntax
    (provide stx))

  (require (for-syntax (submod "stx.rkt" untyped)))
  (require "untyped-only/syntax-parse.rkt")
  
  (define-syntax (define-and-for-syntax stx)
    (syntax-case stx ()
      [(_ id value)
       (remove-use-site-scope
        #'(begin
            (define-for-syntax id value)
            (define id value)))]))
        
  
  (require (rename-in syntax/parse
                      [define/syntax-parse define/with-parse])
           syntax/parse/define
           syntax/parse/experimental/template
           (for-syntax racket/syntax
                       racket/stxparam)
           (for-meta 2 racket/base racket/syntax)
           racket/stxparam)
  
  (require "typed-untyped.rkt"
           (for-syntax "typed-untyped.rkt"))
  (require-typed/untyped "backtrace.rkt")
  (begin-for-syntax (require-typed/untyped "backtrace.rkt"))
  
  (define-syntax ~maybe
    (pattern-expander
     (λ (stx)
       (syntax-parse stx
         [(_ pat ...)
          #'(~optional (~seq pat ...))]))))
  
  (define-syntax ~maybe*
    (pattern-expander
     (λ (stx)
       (syntax-parse stx
         [(_ name pat ...)
          #'(~and name (~optional (~seq pat ...)))]))))

  (define-for-syntax ((|make ~*kw| base-pattern name?) stx)
    (syntax-case stx ()
      [(_ kw pat ...)
       (keyword? (syntax-e #'kw))
       (let ()
         (define/with-syntax name
           (format-id #'kw "~a" (keyword->string (syntax-e #'kw))))
         #`(#,base-pattern (~seq (~and name kw) pat ...)
                           #,@(if name?
                                  #`(#:name #,(format "the ~a keyword"
                                                      (syntax-e #'kw)))
                                  #'())))]))
  
  (define-syntax ~optkw
    (pattern-expander
     (|make ~*kw| #'~optional #f)))

  (define-syntax ~oncekw
    (pattern-expander
     (|make ~*kw| #'~once #f)))

  (define-syntax ~optkw…
    (pattern-expander
     (|make ~*kw| #'~optional #t)))

  (define-syntax ~oncekw…
    (pattern-expander
     (|make ~*kw| #'~once #t)))
  
  (define-syntax ~kw
    (pattern-expander
     (λ (stx)
       (syntax-parse stx
         [(_ kw:keyword)
          (define/with-syntax name
            (format-id #'kw "~a" (keyword->string (syntax-e #'kw))))
          #'(~and name kw)]))))
  
  ;; Circumvent the bug that causes "syntax-parse: duplicate attribute in: a" in
  ;; (syntax-parse #'(x y z) [((~or a (a b c)) ...) #'(a ...)])
  (define-syntax ~or-bug
    (pattern-expander
     (λ (stx)
       (syntax-parse stx
         [(_ pat ...)
          #'(~and (~or pat ...))]))))
  
  (define-syntax ~lit
    (pattern-expander
     (λ (stx)
       (syntax-parse stx
         [(self (~optional (~seq name:id (~literal ~))) lit)
          (if (attribute name)
              #'(~and name (~literal lit))
              #'(~literal lit))]
         [(self (~optional (~seq name:id (~literal ~))) lit ...)
          (define (s stx) (datum->syntax #'self stx stx stx))
          (if (attribute name)
              #'(~and name (~seq (~literal lit) ...))
              #'(~seq (~literal lit) ...))]))))

  (define-syntax ~with
    (pattern-expander
     (λ (stx)
       (syntax-parse stx
         [(_ pat val)
          #'(~parse pat val)]))))

  (define-syntax ~attr
    (pattern-expander
     (λ (stx)
       (syntax-parse stx
         [(_ attr-name val)
          #'(~bind [attr-name val])]))))
  
  (require (submod ".." m-stx-identifier)
           (for-syntax (submod ".." m-stx-identifier)))
  
  ;; TODO: try to factor out the common parts of these definitions (problem:
  ;; the same code is used at different meta-levels, we would need a separate
  ;; module to declare it).
  (define-simple-macro (define-syntax/parse (name . args) body0 . body)
    (define-syntax (name stx2)
      (with-backtrace (syntax->datum stx2)
        (syntax-parameterize ([stx (make-rename-transformer #'stx2)])
          (syntax-parse stx2
            [(_ . args) body0 . body])))))
  
  (define-syntax-rule (define-syntax/case (name . args) literals body0 . body)
    (define-syntax (name stx2)
      (syntax-parameterize ([stx (make-rename-transformer #'stx2)])
        (syntax-case stx2 literals
          [(_ . args) (let () body0 . body)]))))
  
  (define-syntax-rule (λ/syntax-parse args . body)
    (λ (stx2)
      (with-backtrace (syntax->datum stx2)
        (syntax-parameterize ([stx (make-rename-transformer #'stx2)])
          (syntax-parse stx2
            [args . body])))))
  
  (define-syntax-rule (λ/syntax-case args literals . body)
    (λ (stx2)
      (with-backtrace (syntax->datum stx2)
        (syntax-parameterize ([stx (make-rename-transformer #'stx2)])
          (syntax-case stx2 literals
            [args (let () . body)])))))
  
  (define-syntax (define-for-syntax/case-args wstx)
    (syntax-case wstx ()
      [(_ (name args ...) . body)
       (with-syntax ([(param ...) (generate-temporaries #'(args ...))])
         #'(define-for-syntax (name param ...)
             (with-syntax ([args param] ...)
               . body)))]))

  (define-syntax (define/case-args wstx)
    (syntax-case wstx ()
      [(_ (name args ...) . body)
       (with-syntax ([(param ...) (generate-temporaries #'(args ...))])
         #'(define (name param ...)
             (with-syntax ([args param] ...)
               . body)))]))
  
  ;; λstx
  (begin
    (define-syntax-rule (λstx (param ...) body ...)
      (λ (param ...)
        (with-syntax ([param param] ...)
          body ...)))
    
    (module+ test
      (require typed/rackunit)
      (check-equal? (syntax->datum ((λstx (foo bar) #'(foo bar)) #'a #'b))
                    (syntax->datum #'(a b)))))
  
  ;; template/loc
  (begin
    (define-syntax-rule (template/loc loc . tmpl)
      (quasisyntax/loc loc #,(template . tmpl))))
  
  ;; quasitemplate/loc
  (begin
    (define-syntax-rule (quasitemplate/loc loc . tmpl)
      (quasisyntax/loc loc #,(quasitemplate . tmpl))))
  
  ;; template/debug
  (begin
    (define-syntax (template/debug stx)
      (syntax-parse stx
        [(_ debug-attribute:id . rest)
         #'((λ (x)
              (when (attribute debug-attribute)
                (pretty-write (syntax->datum x)))
              x)
            (template . rest))])))
  
  ;; quasitemplate/debug
  (begin
    (define-syntax (quasitemplate/debug stx)
      (syntax-parse stx
        [(_ debug-attribute:id . rest)
         #'((λ (x)
              (when (attribute debug-attribute)
                (pretty-write (syntax->datum x)))
              x)
            (quasitemplate . rest))])))
  
  ;; meta-eval
  (begin
    ;; TODO: this is kind of a hack, as we have to write:
    #;(with-syntax ([(x ...) #'(a bb ccc)])
        (let ([y 70])
          (quasitemplate
           ([x (meta-eval (+ #,y (string-length
                                  (symbol->string
                                   (syntax-e #'x)))))]
            ...))))
    ;; Where we need #,y instead of using:
    ;; (+ y (string-length etc.)).
    (module m-meta-eval racket
      (provide meta-eval)
      (require syntax/parse/experimental/template)
      
      (define-template-metafunction (meta-eval stx)
        (syntax-case stx ()
          [(_ . body)
           #`#,(eval #'(begin . body))])))
    (require 'm-meta-eval))

  (define-syntax (identity-macro stx)
    (syntax-case stx ()
      [(_ . rest)
       (remove-use-site-scope #'rest)]))

  (module m-name-or-curry racket/base
    (provide (all-defined-out))
    (require syntax/parse)
    (define-syntax-class name-or-curry
      #:attributes (id)
      (pattern id:id)
      (pattern (:name-or-curry . curry-args))))
  (require 'm-name-or-curry)

  (define (match-id [rx : Regexp] [id : Identifier])
    (let ([m (regexp-match rx (symbol->string (syntax-e id)))])
      (and m (map (λ ([% : (U #f String)])
                    (and % (datum->syntax id (string->symbol %) id id)))
                  (cdr m)))))
  (define-syntax ~rx-id
    (pattern-expander
     (λ (stx)
       (syntax-case stx ()
         [(_ rx . g*)
          #'(~and x:id
                  {~parse g* (match-id rx #'x)})])))))
