#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/set]]
@title{set}
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}
@defmodule[phc-toolkit/set
           #:use-sources
           [(submod (lib "phc-toolkit/set.rkt") typed)]]

@include-section{set-untyped.scrbl}
