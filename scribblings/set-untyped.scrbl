#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/set]]
@(def-orig typed [phc-toolkit/set])
@title{Untyped versions of set}
@defmodule[phc-toolkit/untyped/set
           #:use-sources
           [(submod (lib "phc-toolkit/set.rkt") untyped)]]

