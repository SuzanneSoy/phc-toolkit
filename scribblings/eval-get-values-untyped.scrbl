#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/eval-get-values]]
@(def-orig typed [phc-toolkit/eval-get-values])
@title{Untyped versions of eval-get-values}
@defmodule[phc-toolkit/untyped/eval-get-values
           #:use-sources
           [(submod (lib "phc-toolkit/eval-get-values.rkt") untyped)]]

