#lang scribble/manual
@require[@for-label[phc-toolkit/stx
                    (only-meta-in 0 phc-toolkit/meta-struct)
                    (only-meta-in 1 phc-toolkit/untyped/meta-struct)
                    racket/base
                    racket/struct-info]]

@title{meta operations on structs}
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}

@section{Typed macros and procedures}

@defmodule[phc-toolkit/meta-struct
           #:use-sources
           [(submod (lib "phc-toolkit/meta-struct.rkt") typed)]]

@defform[(struct-predicate s)
         #:grammar [[s meta-struct?]]]{
 Expands to a predicate for the given @racket[struct], with the
 type @racket[(-> any/c boolean? : s)].}

@defform[(struct-constructor s)
         #:grammar [[s meta-struct?]]]{
 This macro expands to the constructor function for the given @racket[struct],
 with the type @racket[(-> _arg … s)] where each @racket[_arg] corresponds to an
 argument expected by the @racket[struct]'s constructor.}

@defform*[{(struct-accessor s i)
           (struct-accessor s field)}
          #:grammar [[s meta-struct?]
                     [i (expr/c exact-nonnegative-integer?)]
                     [field identifier?]]]{
 This macro expands to the @racket[i]-th accessor function for the given
 @racket[struct], with the type @racket[(-> s _t)] where @racket[_t] is the
 @racket[struct]'s @racket[_i]-th field's type.

 If the second argument is an identifier, then this macro concatenates the
 identifiers @racket[s] and @racket[field] with a @racket[-] in between, and
 expands to the resulting @racket[_s-field]. The lexical context of
 @racket[_s-field] is the same as the lexical context of @racket[s]. In some
 rare cases this might not resolve to the accessor for @racket[field] on
 @racket[s]. Passing an @racket[exact-nonnegative-integer?] as the second
 argument should be more reliable.}

@defproc[#:kind "phase 1 procedure"
         (struct-type-is-immutable? [st Struct-TypeTop])
         boolean?]{
 Returns @racket[#t] if the given struct type can be determined
 to have only immutable fields. Returns @racket[#f] otherwise.}

@defproc[(struct-instance-is-immutable? [v struct?])
         boolean?]{
 Returns @racket[#t] if @racket[v] can be determined to be an instance of an
 immutable struct. Returns @racket[#f] otherwise. Note that when given an
 instance of an opaque struct @racket[struct-instance-is-immutable?] cannot
 access the struct info, and therefore returns @racket[#f].}

@include-section{meta-struct-untyped.scrbl}

@section{Untyped for-syntax utilities}

@defmodule[phc-toolkit/untyped/meta-struct
           #:use-sources
           [(submod (lib "phc-toolkit/meta-struct.rkt") untyped)]]

@defproc[(meta-struct? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] can be used by the
 functions provided by this module, and @racket[#f]
 otherwise. More precisely, @racket[v] must be an 
 @racket[identifier] whose @racket[syntax-local-value] is a
 @racket[struct-info?].

 @history[#:changed "1.0" "This function is provided at phase 1."]}

@defstruct[meta-struct-info ([type-descriptor (or/c identifier? #f)]
                             [constructor (or/c identifier? #f)]
                             [predicate (or/c identifier? #f)]
                             [accessors (list*of identifier?
                                                 (or/c (list/c #f) null?))]
                             [mutators (list*of (or/c identifier? #f)
                                                (or/c (list/c #f) null?))]
                             [super-type (or/c identifier? #f)])]{
 Encapsulates the result of @racket[extract-struct-info] in
 a structure with named fields, instead of an obscure
 six-element list. The precise contents of each field is
 described in 
 @secref["structinfo" #:doc '(lib "scribblings/reference/reference.scrbl")].

 @history[#:changed "1.0" "The identifiers are provided at phase 1."]}

@defproc[(get-meta-struct-info [s meta-struct?]
                               [#:srcloc srcloc (or/c #f syntax?) #f])
         meta-struct-info?]{
 Returns the @racket[meta-struct-info] for the given
 identifier. The optional @racket[#:srcloc] keyword argument
 gives the source location for error messages in case the
 given identifier is not a @racket[meta-struct?].

 @history[#:changed "1.0" "This function is provided at phase 1."]}
 
@defproc[(meta-struct-subtype? [sub meta-struct?] [super meta-struct?])
         boolean?]{
 Returns @racket[#t] if the @racket[struct] associated to
 the identifier @racket[sub] is a subtype of the 
 @racket[struct] associated to the identifier 
 @racket[super], and @racket[#f] otherwise or if the current
 inspector is not strong enough to know.

 @history[#:changed "1.0" "This function is provided at phase 1."]}

@defproc[#:kind "phase 1 procedure"
         (struct-type-id-is-immutable? [id identifier?])
         boolean?]{
 Returns @racket[#t] if the struct with the given @racket[id] can be determined
 to have only immutable fields. Returns @racket[#f] otherwise.}

@(require (for-syntax racket/base
                      racket/syntax
                      racket/struct
                      racket/vector))

@(define-for-syntax (strip-loc e)
   (cond [(syntax? e) (datum->syntax e (strip-loc (syntax-e e)) #f)]
         [(pair? e) (cons (strip-loc (car e)) (strip-loc (cdr e)))]
         [(vector? e) (vector-map strip-loc e)]
         [(box? e) (box (strip-loc (unbox e)))]
         [(prefab-struct-key e)
          => (λ (k) (apply make-prefab-struct
                           k
                           (strip-loc (struct->list e))))]
         [else e]))

@(define-syntax (shorthand stx)
   (syntax-case stx ()
     [(_ base expresion-type)
      (with-syntax ([loc (datum->syntax #'base #'base #f)]
                    [name (format-id #'base "meta-struct-~a" #'base)]
                    [accessor (format-id #'base "meta-struct-info-~a" #'base)]
                    [tmpl (format-id #'base "!struct-~a" #'base)])
        #`(deftogether
            [(defproc (name [s meta-struct?]
                            [#:srcloc srcloc (or/c #f syntax?) #f])
               (expressionof
                (→ s #,(strip-loc #'expresion-type))))
             (defform #:kind "template metafunction"
               (tmpl #,(strip-loc #'s) #,(strip-loc #'maybe-srcloc))
               #:grammar ([s meta-struct?]
                          [maybe-srcloc (code:line)
                           #||#         (code:line #:srcloc srcloc)]))]
            @list{
       @;{}   Shorthand for @racket[(accessor (get-meta-struct-info s))]
       @;{}   (with the optional @racket[#:srcloc] passed to
       @;{}   @racket[get-meta-struct-info]). The precise contents of the
       @;{}   returned value field is described in 
       @;{}   @secref["structinfo"
                      #:doc '(lib "scribblings/reference/reference.scrbl")].
       @;{}
       @;{}   @history[#:changed "1.0"
                       "This function is provided at phase 1."]}))]))

@(shorthand type-descriptor (or/c identifier? #f))
@(shorthand constructor (or/c identifier? #f))
@(shorthand predicate (or/c identifier? #f))
@(shorthand accessors (list*of identifier?
                               (or/c (list/c #f) null?)))
@(shorthand mutators (list*of (or/c identifier? #f)
                              (or/c (list/c #f) null?)))
@(shorthand super-type (or/c identifier? #f))
