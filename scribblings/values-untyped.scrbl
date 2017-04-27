#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/values]]
@(def-orig typed [phc-toolkit/values])
@title{Untyped versions of values}
@defmodule[phc-toolkit/untyped/values
           #:use-sources
           [(submod (lib "phc-toolkit/values.rkt") untyped)]]

