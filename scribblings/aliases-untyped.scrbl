#lang scribble/manual
@require["utils.rkt"
         @for-label[phc-toolkit/untyped/aliases
                    racket/base
                    (only-in racket ... compose)
                    racket/match
                    syntax/parse]]
@(def-orig orig [racket/syntax]
   generate-temporary)

@title{Untyped versions of the aliases}
@defmodule[phc-toolkit/untyped/aliases
           #:use-sources
           [phc-toolkit/untyped/aliases]]

@defidform[∘]{An alias for @racket[compose]}
@defidform[…]{An alias for @racket[...]}
@defidform[…+]{An alias for @racket[...+]}
@defidform[match-λ]{An alias for @racket[match-lambda]}
@defidform[match-λ*]{An alias for @racket[match-lambda*]}
@defidform[match-λ**]{An alias for @racket[match-lambda**]}
@defidform[generate-temporary]{Equivalent to @orig:generate-temporary (but not
 @racket[free-identifier=?] to the original for now)}
@defidform[attr]{An alias for @racket[attribute] which also works for plain
 syntax pattern variables}
@defidform[|@|]{An alias for @racket[attribute] which also works for plain
 syntax pattern variables}
@defform[(when-attr name expr)]{
 Equivalent to @racket[(if (attribute name) expr #'())]}