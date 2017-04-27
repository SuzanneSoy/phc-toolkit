#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/logn-id]]
@(def-orig typed [phc-toolkit/logn-id])
@title{Untyped versions of logn-id}
@defmodule[phc-toolkit/untyped/logn-id
           #:use-sources
           [(submod (lib "phc-toolkit/logn-id.rkt") untyped)]]

