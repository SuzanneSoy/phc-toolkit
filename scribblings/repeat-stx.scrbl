#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/repeat-stx]]
@title{repeat-stx}
@author{@author+email["Suzanne Soy" "racket@suzanne.soy"]}
@defmodule[phc-toolkit/repeat-stx
           #:use-sources
           [(submod (lib "phc-toolkit/repeat-stx.rkt") typed)]]

@include-section{repeat-stx-untyped.scrbl}
