#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/test-framework]]
@title{test-framework}
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}
@defmodule[phc-toolkit/test-framework
           #:use-sources
           [(submod (lib "phc-toolkit/test-framework.rkt") typed)]]

@include-section{test-framework-untyped.scrbl}
