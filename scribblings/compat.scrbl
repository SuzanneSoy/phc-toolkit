#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/compat]]
@title{Compatibility wrappers}
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}
@defmodule[phc-toolkit/compat
           #:use-sources
           [(submod (lib "phc-toolkit/compat.rkt") typed)]]

@defproc[(record-disappeared-uses* [ids : (U Identifier (Listof Identifier))])
         Any]{
 On Racket 6.5, @racket[record-disappeared-uses] only accepted a list
 of identifiers, not a single identifier on its own. This wrapper allows
 passing a single identifier on Racket 6.5 too.}

@defform[(with-disappeared-uses* . body)]{
 On Racket 6.5, @racket[with-disappeared-uses] allowed a single body
 expression. This wrapper wraps the @racket[body] expressions with a
 @racket[let] form, so that multiple expressions and definitions can be used as
 the body of @racket[with-disappeared-uses*] on Racket 6.5 too.}

@include-section{compat-untyped.scrbl}
