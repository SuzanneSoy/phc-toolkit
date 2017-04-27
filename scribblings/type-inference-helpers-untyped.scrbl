#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/type-inference-helpers]]
@(def-orig typed [phc-toolkit/type-inference-helpers])
@title{Untyped versions of type-inference-helpers}
@defmodule[phc-toolkit/untyped/type-inference-helpers
           #:use-sources
           [(submod (lib "phc-toolkit/type-inference-helpers.rkt") untyped)]]

