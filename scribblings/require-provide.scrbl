#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/require-provide]]
@title{require-provide}
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}
@defmodule[phc-toolkit/require-provide
           #:use-sources
           [(submod (lib "phc-toolkit/require-provide.rkt") typed)]]

@include-section{require-provide-untyped.scrbl}
