#lang scribble/manual
@require["utils.rkt"
         @for-label[phc-toolkit/aliases
                    racket/base
                    (only-in racket ... compose)
                    racket/match
                    syntax/parse]]
@(def-orig orig [racket/syntax]
   generate-temporary)

@title{Aliases for other racket identifiers}
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}
@defmodule[phc-toolkit/aliases
           #:use-sources
           [phc-toolkit/aliases]]

@defidform[∘]{An alias for @racket[compose]}
@defidform[…]{An alias for @racket[...]}
@defidform[…+]{An alias for @racket[...+]}
@defidform[match-λ]{An alias for @racket[match-lambda]}
@defidform[match-λ*]{An alias for @racket[match-lambda*]}
@defidform[match-λ**]{An alias for @racket[match-lambda**]}
@defidform[generate-temporary]{Typed version of @orig:generate-temporary}
@defidform[attr]{An alias for @racket[attribute] which also works for plain
 syntax pattern variables}
@defidform[|@|]{An alias for @racket[attribute] which also works for plain
 syntax pattern variables}
@defform[(when-attr name expr)]{
 Equivalent to @racket[(if (attribute name) expr #'())]}

@include-section{aliases-untyped.scrbl}
