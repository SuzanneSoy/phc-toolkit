#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test #:untyped-first
  (provide !temp
           (rename-out [!temp &])
           format-ids
           hyphen-ids
           format-temp-ids
           #|!temp|#
           define-temp-ids)
  
  (require "typed-untyped.rkt"
           "untyped-only/syntax-parse.rkt")
  (require-typed/untyped "sequence.rkt")
  (if-typed (require phc-toolkit/aliases)
            (require phc-toolkit/untyped/aliases))
  (begin-for-syntax (require "typed-untyped.rkt"
                             "untyped-only/format-id-record.rkt")
                    (if-typed (require phc-toolkit/aliases)
                              (require phc-toolkit/untyped/aliases)))
  
  (module m-!temp racket
    (provide !temp)
    
    (require syntax/parse
             syntax/parse/experimental/template)
    
    (define-template-metafunction (!temp stx)
      (syntax-parse stx
        [(_ id:id)
         #:with (temp) (generate-temporaries #'(id))
         #'temp]
        #|[(_ . id:id)
       #:with (temp) (generate-temporaries #'(id))
       #'temp]
      [(_ id:id ...)
       (generate-temporaries #'(id ...))]|#)))
  (require 'm-!temp)
  
  (require/typed racket/syntax
                 [format-id (→ Syntax String (U String Identifier) *
                               Identifier)])
  (require (only-in racket/syntax define/with-syntax)
           (only-in syntax/stx stx-map)
           (for-syntax racket/base
                       racket/format
                       racket/syntax
                       syntax/parse
                       syntax/parse/experimental/template))
  ;(require racket/sequence) ;; in-syntax
  
  (define-type S-Id-List
    (U String
       Identifier
       (Listof String)
       (Listof Identifier)
       (Syntaxof (Listof Identifier))))
  
  ; TODO: format-ids doesn't accept arbitrary values. Should we change it?
  ; 
  (: format-ids (→ (U Syntax (→ (U String Identifier) * Syntax))
                   String
                   S-Id-List *
                   (Listof Identifier)))
  (define (format-ids lex-ctx format . vs)
    (let* ([seqs
            (map (λ ([v : S-Id-List])
                   (cond
                     [(string? v) (in-cycle (in-value v))]
                     [(identifier? v) (in-cycle (in-value v))]
                     [(list? v) (in-list v)]
                     [else (in-list (syntax->list v))]))
                 vs)]
           [justconstants (andmap (λ (x) (or (string? x) (identifier? x))) vs)]
           [seqlst (apply sequence-list seqs)])
      (for/list : (Listof Identifier)
        ([items seqlst]
         [bound-length (if justconstants
                           (in-value 'yes)
                           (in-cycle (in-value 'no)))])
        
        (apply format-id
               (if (procedure? lex-ctx) (apply lex-ctx items) lex-ctx)
               format
               items))))
  
  (: hyphen-ids (→ (U Syntax (→ (U String Identifier) * Syntax))
                   S-Id-List *
                   (Listof Identifier)))
  
  (define (hyphen-ids lex-ctx . vs)
    (apply format-ids
           lex-ctx
           (string-join (map (λ _ "~a") vs) "-")
           vs))
  
  (: format-temp-ids (→ String
                        S-Id-List *
                        (Listof Identifier)))
  
  (define (format-temp-ids format . vs)
    ;; Introduce the binding in a fresh scope.
    (apply format-ids
           (λ _ ((make-syntax-introducer) (if (syntax? format)
                                              format
                                              (datum->syntax #f '()))))
           format
           vs))

  (: to-identifier (→ Any Identifier))
  (define (to-identifier v)
    (cond
      [(identifier? v) v]
      [(syntax? v) (datum->syntax v (to-symbol (syntax-e v)))]
      [else (datum->syntax #f (to-symbol v))]))

  (: to-symbol (→ Any Symbol))
  (define (to-symbol v)
    (cond
      [(symbol? v) v]
      [(string? v) (string->symbol v)]
      [(number? v) (string->symbol (format "~a" v))]
      [else (syntax-e (generate-temporary v))]))

  (: generate-string (→ String))
  (define (generate-string)
    (symbol->string
     (syntax-e
      (generate-temporary ""))))

  (require (for-syntax (submod "stx.rkt" untyped)))

  
  (: curried-map-on-attribute-step
     (∀ (A B) (→ (→ A B)
                 (case→ (→ #f #f)
                        (→ (Listof A) (Listof B))
                        (→ (U #f (Listof A))
                           (U #f (Listof B)))))))
  (define ((curried-map-on-attribute-step f) l)
    (if (eq? l #f)
        l
        (map f l)))

  (: curried-map-on-attribute-last
     (∀ (A B) (→ (→ (Syntaxof A) B)
                 (case→ (→ #f #f)
                        (→ (Syntaxof A) B)
                        (→ (U #f (Syntaxof A)) (U #f B))))))
  (define ((curried-map-on-attribute-last f) v)
    (if (eq? v #f)
        v
        (f v)))

  ;; (map-on-attribute f depth)
  (define-syntax (map-on-attribute stx)
    (syntax-case stx ()
      [(_ f 0)
       #'(curried-map-on-attribute-last f)]
      [(_ f depth)
       #`(curried-map-on-attribute-step
          (map-on-attribute f
                            #,(sub1 (syntax-e #'depth))))]))

  (begin-for-syntax
    (define-syntax-class dotted
      (pattern id:id
               #:attr make-dotted
               (λ (x) x)
               #:attr wrap
               (λ (x f) (f x #t))
               #:attr depth 0
               #:with stx-depth #'0)
      (pattern (nested:dotted (~literal ...));(~and dots (~literal ...)) ...+)
               #:with id #'nested.id
               #:attr make-dotted
               (λ (x) #`(#,((attribute nested.make-dotted) x) (... ...)))
               #:attr wrap
               (λ (x f) (f ((attribute nested.wrap) x f) #f))
               #:attr depth (add1 (attribute nested.depth))
               #:with stx-depth #`#,(add1 (attribute nested.depth))))
    
    (define-syntax-class simple-format
      (pattern format
               #:when (string? (syntax-e #'format))
               #:when (regexp-match #rx"^([^~]|~~)*~a([^~]|~~)*$"
                                    (syntax-e #'format)))))

  ;; This macro should really be heavily refactored.
  ;; TODO: merge all cases thanks to format-id/record and syntax classes.
  (define-syntax (define-temp-ids stx)
    (with-arrows
     (syntax-parse stx
       #|
      ;; TODO : factor this with the next case.
      [(_ format ((base:id (~literal ...)) (~literal ...)))
       #:when (string? (syntax-e #'format))
       (with-syntax ([pat (format-id #'format (syntax-e #'format) #'base)])
         #'(define/with-syntax ((pat (... ...)) (... ...))
             (stx-map (curry format-temp-ids format)
                      #'((base (... ...)) (... ...)))))]
|#

       ;; Multiple formats
       [(_ {~and {~optional #:concise} {~seq maybe-concise …}}
           (format:simple-format …)
           (~and (~seq options …)
                 (~seq base:dotted
                       (~or (~seq #:first-base first-base)
                            (~optional (~seq #:first first)))
                       (~optional (~seq #:prefix prefix)))))
        #'(begin (define-temp-ids maybe-concise … format options …) …)]

       ;; New features (arrows and #:first) special-cased for now
       ;; TODO: make these features more general.

       ;; With #:first-base, translated to #:first
       [(_ {~and {~optional #:concise} {~seq maybe-concise …}}
           format:simple-format base:dotted
           #:first-base first-base
           (~optional (~seq #:prefix prefix)))
        #:with first (format-id/record #'format #'format #'first-base)
        (template
         (define-temp-ids maybe-concise … format base
           #:first first
           (?? (?@ #:prefix prefix))))]

       ;; Base case with a simple format "...~a...".
       [(_ {~optional {~and #:concise concise?}}
           format:simple-format
           base:dotted
           (~optional (~seq #:first first))
           (~optional (~seq #:first… first…))
           (~optional (~seq #:prefix prefix)))
        (let* ([base-len (string-length (symbol->string (syntax-e #'base.id)))])
          (define/with-syntax pat
            (format-id/record #'format #'format #'base.id))
          (define/with-syntax pat-dotted ((attribute base.make-dotted) #'pat))

          (define/with-syntax maybe-generate-temporary
            (if (attribute concise?)
                #'to-identifier
                #'generate-temporary))
          (define/with-syntax format-temp-ids-last
            (template
             (λ (x)
               (car (format-temp-ids (?? (?@ (string-append "~a:" format) prefix)
                                         format)
                                     (maybe-generate-temporary x))))))
          (define/with-syntax format-temp-ids*
            #'(map-on-attribute format-temp-ids-last base.stx-depth))
          (define/with-syntax (tmp-valvar) (generate-temporaries #`(base.id)))
          (define/with-syntax do-define-pat
            (syntax-parse (attribute-info #'base.id '(pvar attr))
              [({~datum attr} valvar depth name syntax?)
               #'(define-raw-attribute pat
                   tmp-valvar
                   (format-temp-ids* valvar)
                   depth
                   syntax?)]
              [({~datum pvar} valvar depth)
               #'(define-raw-syntax-mapping pat
                   tmp-valvar
                   (format-temp-ids* valvar)
                   depth)]))
          (define/with-syntax do-define-first…
            (if (attribute first…)
                (let ()
                  (define/with-syntax (tmp-first-valvar)
                    (generate-temporaries #`(base.id)))
                  (syntax-parse (attribute-info #'base.id '(pvar attr))
                    [({~datum attr} valvar depth name syntax?)
                     ;; TODO: always define an attribute, but don't use
                     ;; define-raw-attribute, instead use the copy-attribute
                     ;; code from subtemplate.
                     #`(define-raw-attribute first…
                         tmp-first-valvar
                         (car tmp-valvar)
                         #,(sub1 (syntax-e #'depth))
                         syntax?)]
                    [({~datum pvar} valvar depth)
                     #`(define-raw-syntax-mapping first…
                         tmp-first-valvar
                         (car tmp-valvar)
                         #,(sub1 (syntax-e #'depth)))]))
                #'(begin)))
          (define/with-syntax do-define-first
            (if (attribute first)
                #'(define/with-syntax (first . _)
                    #'pat-dotted)
                #'(begin)))
          #'(begin do-define-pat
                   do-define-first
                   do-define-first…))]

       ;; Simplistic handling when the format contains no ~ at all.
       ;; (TODO: should allow ~~)
       [(_ {~optional {~and #:concise concise?}} format base:dotted)
        #:when (string? (syntax-e #'format))
        #:when (regexp-match #rx"^([^~]|~~)*$" (syntax-e #'format))
        (define/with-syntax pat (format-id/record #'format #'format))
        (define/with-syntax pat-dotted ((attribute base.make-dotted) #'pat))
        (define/with-syntax format-temp-ids*
          ((attribute base.wrap) #`(λ (x)
                                     #,(if (attribute concise?)
                                           #'(car (format-temp-ids
                                                   (string-append format)))
                                           #'(car (format-temp-ids
                                                   (string-append format "-~a")
                                                   (generate-string)))))
                                 (λ (x deepest?)
                                   (if deepest?
                                       x
                                       #`(curry stx-map #,x)))))
        #'(define/with-syntax pat-dotted
             (format-temp-ids* #'base))]

       ;; Very simplistic handling when the name is explicitly given.
       [(_ {~optional {~and #:concise concise?}}
           name:id format:expr . vs)
        #`(define/with-syntax name (format-temp-ids format . vs))]))))