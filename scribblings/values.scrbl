#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/values]]
@title{values}
@author{@author+email["Suzanne Soy" "racket@suzanne.soy"]}
@defmodule[phc-toolkit/values
           #:use-sources
           [(submod (lib "phc-toolkit/values.rkt") typed)]]

@include-section{values-untyped.scrbl}
