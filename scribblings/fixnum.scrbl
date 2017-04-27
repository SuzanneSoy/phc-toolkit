#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/fixnum]]
@(def-orig orig [racket/fixnum] fxxor)
@title{Fixnum operations (fxxor …)}
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}
@defmodule[phc-toolkit/fixnum
           #:use-sources
           [(submod (lib "phc-toolkit/fixnum.rkt") typed)]]

@defproc[(fxxor2 [a Fixnum] [b Fixnum]) Fixnum]{
 @orig:fxxor from @racketmodname[racket/fixnum], re-provided with the type
 @racket[(Fixnum Fixnum → Fixnum)].}

@defproc[(fxxor [a Fixnum] ...) Fixnum]{
 N-aray generalization or @racket[fxxor2]. Equivalent to
 @racket[(foldl fxxor2 0 args)].}

@include-section{fixnum-untyped.scrbl}
