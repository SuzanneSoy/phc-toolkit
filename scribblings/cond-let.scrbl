#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/cond-let]]
@title{cond-let}
@author{@author+email["Suzanne Soy" "racket@suzanne.soy"]}
@defmodule[phc-toolkit/cond-let
           #:use-sources
           [(submod (lib "phc-toolkit/cond-let.rkt") typed)]]

@include-section{cond-let-untyped.scrbl}
