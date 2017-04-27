#lang scribble/manual
@require[racket/require
         "utils.rkt"
         scribble/struct
         scribble/decode
         @for-label[phc-toolkit/untyped-only/format-id-record
                    phc-toolkit/stx
                    racket/syntax
                    syntax/parse
                    racket/contract
                    racket/base]]
@title[#:tag "phc-toolkit-format-id-record"]{Formatting identifiers so that
 DrRacket still shows arrows}
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}
@defmodule[phc-toolkit/untyped-only/format-id-record
           #:use-sources
           [(lib "phc-toolkit/untyped-only/format-id-record.rkt")]]

@defproc[(format-id/record [lctx (or/c syntax? #f)]
                           [fmt (stx-e/c
                                 (and/c string?
                                        (regexp-match/c "^([^~]|~~|~a|~A)*$")))]
                           [#:source src (or/c syntax? #f) #f]
                           [#:props props (or/c syntax? #f) #f]
                           [vs (or/c string? symbol? keyword? char? number?
                                     (syntax/c string?)
                                     identifier?
                                     (syntax/c keyword?)
                                     (syntax/c char?)
                                     (syntax/c number?))]
                           ...)
         identifier?]{
 Like @racket[format-id], but cooperates with @racket[with-sub-range-binders]
 to record sub-range binders, which allow DrRacket to draw arrows from the
 identifiers present in @racket[vs ...] to occurrences of the resulting
 identifier. It also means that when one or more identifiers present in
 @racket[vs ...] are concatenated with other strings, it is possible to rename
 parts of the resulting identifier in DrRacket.

 If @racket[fmt] is a syntax object containing a string, then arrows are drawn
 from the format itself to the generated identifier, for each part of the
 format which appears in the identifier (e.g. if the format is
 @racket["x~~y~az"], then two arrows will be drawn from the format, one for
 @racket["x~~y"], and one for @racket["z"].

 This function must be called within the dynamic extent of
 @racket[with-sub-range-binders] or @racket[with-arrows].}

@defform[(with-sub-range-binders body-expr ... stx-expr)]{
 The value produced by @racket[stx-expr] must be a syntax object. All
 @seclink["Syntax_Properties_that_Check_Syntax_Looks_For"
          #:doc '(lib "scribblings/tools/tools.scrbl")]{sub-range binders}
 recorded via @racket[record-sub-range-binders!] or
 @racket[maybe-record-sub-range-binders!] are added to the syntax object in a
 @seclink["Syntax_Properties_that_Check_Syntax_Looks_For"
          #:doc '(lib "scribblings/tools/tools.scrbl")
          ]{@racket['sub-range-binders]} property.
}

@defform[(with-arrows body-expr ... stx-expr)]{
 Equivalent to:
 
 @racketblock[(with-disappeared-uses
               (with-sub-range-binders
                body-expr ... stx-expr))]}

@defform[(syntax-parser-with-arrows . syntax-parser-options+clauses)]{
 Equivalent to:
 
 @racketblock[(λ (stx)
                (with-arrows
                 ((syntax-parser . syntax-parser-options+clauses) stx)))]

 Within the @racket[syntax-parser-options+clauses], it is possible to use the
 @racket[stx] identifier to refer to the whole syntax, in addition to using
 @racket[syntax/parse]'s @racket[this-syntax].}

@defproc[(record-sub-range-binders! [sub-range-binders
                                     (or/c sub-range-binder/c
                                           (listof sub-range-binder/c))])
         void?]{
 Cooperates with the enclosing @racket[with-sub-range-binders] or
 @racket[with-arrows] to record the given sub-range-binders so that they are
 added to the syntax object returned by @racket[with-sub-range-binders] or
 @racket[with-arrows].

 This function must be called within the dynamic extent of
 @racket[with-sub-range-binders] or @racket[with-arrows].}

@defproc[(maybe-record-sub-range-binders! [sub-range-binders
                                           (or/c sub-range-binder/c
                                                 (listof sub-range-binder/c))])
         void?]{
 Cooperates with the enclosing @racket[with-sub-range-binders] or
 @racket[with-arrows] to record the given sub-range-binders so that they are
 added to the syntax object returned by @racket[with-sub-range-binders] or
 @racket[with-arrows].

 If this function is not called within the dynamic extent of
 @racket[with-sub-range-binders] or @racket[with-arrows], it has no effect and
 the sub-range-binders are not recorded.}

@defparam[current-recorded-sub-range-binders sub-range-binders
          (or/c (listof sub-range-binder/c) false/c)]{
 This parameter contains the list of sub-range-binders recorded so far by the
 nearest @racket[with-sub-range-binders] or @racket[with-arrows].}

@defthing[sub-range-binder/c chaperone-contract?
          #:value
          (or/c (vector/c syntax?
                          exact-nonnegative-integer? exact-nonnegative-integer?
                          (real-in 0 1) (real-in 0 1)
                          syntax?
                          exact-nonnegative-integer? exact-nonnegative-integer?
                          (real-in 0 1) (real-in 0 1))
                (vector/c syntax?
                          exact-nonnegative-integer? exact-nonnegative-integer?
                          syntax?
                          exact-nonnegative-integer? exact-nonnegative-integer?)
                )]{
 A contract accepting valid representations of
 @seclink["Syntax_Properties_that_Check_Syntax_Looks_For"
          #:doc '(lib "scribblings/tools/tools.scrbl")]{sub-range binders}.
}