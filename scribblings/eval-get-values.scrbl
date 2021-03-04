#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/eval-get-values]]
@title{eval-get-values}
@author{@author+email["Suzanne Soy" "racket@suzanne.soy"]}
@defmodule[phc-toolkit/eval-get-values
           #:use-sources
           [(submod (lib "phc-toolkit/eval-get-values.rkt") typed)]]

@include-section{eval-get-values-untyped.scrbl}
