#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/list]]
@title{list}
@author{@author+email["Suzanne Soy" "racket@suzanne.soy"]}
@defmodule[phc-toolkit/list
           #:use-sources
           [(submod (lib "phc-toolkit/list.rkt") typed)]]

@include-section{list-untyped.scrbl}
