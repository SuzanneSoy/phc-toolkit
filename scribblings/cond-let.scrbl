#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/cond-let]]
@title{cond-let}
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}
@defmodule[phc-toolkit/cond-let
           #:use-sources
           [(submod (lib "phc-toolkit/cond-let.rkt") typed)]]

@include-section{cond-let-untyped.scrbl}
