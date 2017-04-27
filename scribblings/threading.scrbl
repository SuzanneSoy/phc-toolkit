#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/threading]]
@title{threading}
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}
@defmodule[phc-toolkit/threading
           #:use-sources
           [(submod (lib "phc-toolkit/threading.rkt") typed)]]

@include-section{threading-untyped.scrbl}
