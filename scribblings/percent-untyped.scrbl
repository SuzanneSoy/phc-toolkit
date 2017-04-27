#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/percent]]
@(def-orig typed [phc-toolkit/percent])
@title{Untyped versions of percent}
@defmodule[phc-toolkit/untyped/percent
           #:use-sources
           [(submod (lib "phc-toolkit/percent.rkt") untyped)]]

