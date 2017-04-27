#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/logn-id]]
@title{logn-id}
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}
@defmodule[phc-toolkit/logn-id
           #:use-sources
           [(submod (lib "phc-toolkit/logn-id.rkt") typed)]]

@include-section{logn-id-untyped.scrbl}
