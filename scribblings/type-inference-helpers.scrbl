#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/type-inference-helpers]]
@title{type-inference-helpers}
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}
@defmodule[phc-toolkit/type-inference-helpers
           #:use-sources
           [(submod (lib "phc-toolkit/type-inference-helpers.rkt") typed)]]

@defform[#:kind "type expander"
         (maybe-apply-type τ arg ...)]{
 Expands to @racket[τ] if there are no arguments, and to @racket[(τ arg ...)]
 if there is at least one argument. }

@include-section{type-inference-helpers-untyped.scrbl}
