#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/generate-indices]]
@title{generate-indices}
@author{@author+email["Suzanne Soy" "racket@suzanne.soy"]}
@defmodule[phc-toolkit/generate-indices
           #:use-sources
           [(submod (lib "phc-toolkit/generate-indices.rkt") typed)]]

@include-section{generate-indices-untyped.scrbl}
