#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/generate-indices]]
@(def-orig typed [phc-toolkit/generate-indices])
@title{Untyped versions of generate-indices}
@defmodule[phc-toolkit/untyped/generate-indices
           #:use-sources
           [(submod (lib "phc-toolkit/generate-indices.rkt") untyped)]]

