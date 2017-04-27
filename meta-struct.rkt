#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (require (for-syntax syntax/parse/experimental/template
                       syntax/parse
                       racket/syntax))

  (begin-for-syntax
    (provide meta-struct?
             (struct-out meta-struct-info)
             get-meta-struct-info
             ;; More provided by `shorthand` in the code below
             meta-struct-subtype?
             struct-type-id-is-immutable?))
  (provide struct-predicate
           struct-constructor
           struct-accessor
           struct-type-is-immutable?
           struct-instance-is-immutable?)

  (module info racket/base
    (require racket/struct-info)
  
    (provide meta-struct?
             (struct-out meta-struct-info)
             get-meta-struct-info)
  
    (define (meta-struct? s)
      (and (identifier? s)
           (let ([v (syntax-local-value s (λ _ #f))])
             (and v (struct-info? v)))))
  
    (struct meta-struct-info
      (type-descriptor
       constructor
       predicate
       accessors
       mutators
       super-type)
      #:transparent)
  
    (define (get-meta-struct-info s
                                  #:srcloc [srcloc #f]
                                  #:fallback [fallback #f])
      (if (meta-struct? s)
          (apply meta-struct-info
                 (extract-struct-info (syntax-local-value s)))
          (if fallback
              (fallback)
              (raise-syntax-error 'get-struct-info
                                  "not a structure definition"
                                  (or srcloc s)
                                  s)))))

  (require 'info
           (for-syntax 'info))

  (define-syntax (shorthand stx)
    (syntax-case stx ()
      [(_ base)
       (with-syntax ([name (format-id #'base "meta-struct-~a" #'base)]
                     [accessor (format-id #'base "meta-struct-info-~a" #'base)]
                     [tmpl (format-id #'base "!struct-~a" #'base)])
         #'(begin-for-syntax
             (provide name tmpl)
             (define-template-metafunction (tmpl stx)
               (syntax-parse stx
                 [(_ s
                     (~optional (~seq #:srcloc srcloc))
                     (~optional (~seq #:fallback fallback)))
                  (accessor
                   (get-meta-struct-info #'s #:srcloc (attribute srcloc)))]))
             (define (name s #:srcloc [srcloc #f] #:fallback [fallback #f])
               (define err (gensym))
               (define val
                 (get-meta-struct-info s
                                       #:srcloc srcloc
                                       #:fallback (and fallback (λ () err))))
               (if (and (eq? val err) fallback)
                   (fallback)
                   (accessor val)))))]))

  (shorthand type-descriptor)
  (shorthand constructor)
  (shorthand predicate)
  (shorthand accessors)
  (shorthand mutators)
  (shorthand super-type)

  (define-syntax (struct-predicate stx)
    (syntax-case stx ()
      [(_ s) (meta-struct-info-predicate (get-meta-struct-info #'s))]))
  (define-syntax (struct-constructor stx)
    (syntax-case stx ()
      [(_ s) (meta-struct-info-constructor (get-meta-struct-info #'s))]))
  (define-syntax (struct-accessor stx)
    (syntax-case stx ()
      [(_ s field)
       (identifier? #'field)
       (begin
         (record-disappeared-uses (list #'s #'field))
         (format-id #'s "~a-~a" #'s #'field))]
      [(_ s i)
       (exact-positive-integer? (syntax-e #'i))
       (list-ref (meta-struct-info-accessors (get-meta-struct-info #'s))
                 (syntax-e #'i))]))

  (define-for-syntax (meta-struct-subtype? sub super)
    (or (equal? (meta-struct-type-descriptor sub)
                (meta-struct-type-descriptor super))
        (let ((up (meta-struct-super-type sub)))
          (and (meta-struct? up)
               (meta-struct-subtype? up super)))))

  (define-for-syntax (struct-type-id-is-immutable? id)
    (andmap not (meta-struct-mutators id)))

  (define (struct-type-is-immutable? [st : Struct-TypeTop]) : Boolean
    (let-values ([(_1 nfields _3 _4 _5 immutable-idx super not-most-specific?)
                  (struct-type-info st)])
      (and (not not-most-specific?)
           (equal? (sort immutable-idx <)
                   (range nfields))
           (if super (struct-type-is-immutable? super) #t))))
  
  (define (struct-instance-is-immutable? v)
    
    (let-values ([(st not-most-specific?) (struct-info v)])
      (and (not not-most-specific?)
           st
           (struct-type-is-immutable? st)))))