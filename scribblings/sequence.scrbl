#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/sequence]]
@title{sequence}
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}
@defmodule[phc-toolkit/sequence
           #:use-sources
           [(submod (lib "phc-toolkit/sequence.rkt") typed)]]

@include-section{sequence-untyped.scrbl}
