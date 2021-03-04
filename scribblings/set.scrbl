#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/set]]
@title{set}
@author{@author+email["Suzanne Soy" "racket@suzanne.soy"]}
@defmodule[phc-toolkit/set
           #:use-sources
           [(submod (lib "phc-toolkit/set.rkt") typed)]]

@include-section{set-untyped.scrbl}
