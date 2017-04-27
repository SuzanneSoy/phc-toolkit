#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/require-provide]]
@(def-orig typed [phc-toolkit/require-provide])
@title{Untyped versions of require-provide}
@defmodule[phc-toolkit/untyped/require-provide
           #:use-sources
           [(submod (lib "phc-toolkit/require-provide.rkt") untyped)]]

