#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/stx
                    phc-toolkit/syntax-parse
                    (subtract-in phc-toolkit/untyped
                                 phc-toolkit/stx
                                 phc-toolkit/syntax-parse)
                    racket/base
                    racket/contract]]

@(def-orig orig [syntax/stx racket/base]
   stx-car
   stx-cdr
   syntax-e)

@title{Syntax object manipulation utilities}
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}

@defmodule[phc-toolkit/stx
           #:use-sources
           [(submod (lib "phc-toolkit/stx.rkt") typed)]]

@; TODO: fix the types
@defproc[(stx-car [v (or/c (syntax/c pair?) pair?)]) any/c]{
 Typed version of @orig:stx-car from @racketmodname[syntax/stx].}

@defproc[(stx-cdr [v (or/c (syntax/c pair?) pair?)]) any/c]{
 Typed version of @orig:stx-cdr from @racketmodname[syntax/stx].}

@defproc[(stx-e [v (or/c (syntax/c any/c) any/c)]) any/c]{
 Typed version of @orig:syntax-e which also accepts objects which are not
 syntax (in which case the original object is returned).}

@defproc[(stx-pair? [v Any]) Boolean]{
 A predicate which returns true for pairs and for syntax pairs alike.
}

@defproc[(stx-car/c [car-c (→ Any Result)]) (→ Any (U #f Result))]{
 Returns a contract similar to the one returned by
 @racket[(cons/c car-c any/c)], but which accepts both syntax pairs
 (@racket[stx-pair?]) and pairs (@racket[pair?]), as long as their
 @racket[stx-car] (@racket[car] respectively) is accepted by @racket[car-c].}

@defproc[(stx-cdr/c [cdr-c (→ Any Result)]) (→ Any (U #f Result))]{
 Returns a contract similar to the one returned by
 @racket[(cons/c any/c cdr-c)], but which accepts both syntax pairs
 (@racket[stx-pair?]) and pairs (@racket[pair?]), as long as their
 @racket[stx-cdr] (@racket[cdr] respectively) is accepted by @racket[cdr-c].}

@defproc[(stx-e/c [e-c (→ Any Result)]) (→ Any (U #f Result))]{
 Equivalent to @racket[(or/c e-c (syntax/c e-c))].

 Also equivalent to @racket[(λ (v) (e-c (stx-e v)))].
                                                                             
 Returns a contract which accepts any value accepted by @racket[e-c]. The
 contract also accepts any value @racket[_v] for which @racket[syntax?] returns
 true and @racket[(syntax-e v)] is accepted by @racket[e-c].}

@defform[#:kind "type"
         (Stx-List? A)]{
 A polymorphic type which is defined as:
 @racketblock[(U Null
                 (Pairof A (Stx-List? A))
                 (Syntaxof Null)
                 (Syntaxof (Pairof A (Stx-List? A))))]}

@defproc[(stx-list? [v Any]) Boolean]{
 A predicate for @racket[Stx-List?].
}

@defproc[(stx->list [l (Stx-List? A)]) (Listof A)]{
 Turns into a list any syntax list, which can be any proper sequence of syntax
 pairs terminated by a syntax list or by @racket[#'()]. If the value @racket[l]
 is already a regular non-syntax list, a copy of the list is returned (note
 that this means that the returned list will most likely not be @racket[eq?] to
 the original).}

@defproc[(stx-list/c [l-c (→ Any Result)]) (→ Any (U #f Result))]{
 Equivalent to:

 @racketblock[
 (λ (v)
   (and (stx-list? v)
        (l-c (stx->list v))))]
                                                                             
 Returns a contract which accepts any list accepted by @racket[l-c]. The
 contract also accepts any value @racket[_v] for which @racket[stx-list?]
 returns true and @racket[(stx->list v)] is accepted by @racket[e-c].}

@defproc[(stx-null? [v Any]) Boolean]{
 Returns @racket[#true] for the empty list (@racket[null]) and for any empty
 syntax list (@racket[#'()]). Returns @racket[#false] for any other value.}

@defproc*[([(stx-assoc
             [id Identifier]
             [alist (Syntaxof (Listof (Syntaxof (Pairof Identifier T))))])
            (U (Syntaxof (Pairof Identifier T)) #f)]
           [(stx-assoc
             [id Identifier]
             [alist (Listof (Syntaxof (Pairof Identifier T)))])
            (U (Syntaxof (Pairof Identifier T)) #f)]
           [(stx-assoc [id Identifier]
                       [alist (Listof (Pairof Identifier T))])
            (U (Pairof Identifier T) #f)])]{
 Like @racket[assoc], but operates on syntax association lists.
}

@defproc*[([(identifier->string [identifier Identifier]) String]
           [(identifier→string [identifier Identifier]) String])]{
 Equivalent to @racket[(symbol->string (syntax-e identifier))].
}

@include-section{stx-untyped-only.scrbl}

@include-section{stx-patching-srcloc.scrbl}

@include-section{stx-untyped.scrbl}
