#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/repeat-stx]]
@(def-orig typed [phc-toolkit/repeat-stx])
@title{Untyped versions of repeat-stx}
@defmodule[phc-toolkit/untyped/repeat-stx
           #:use-sources
           [(submod (lib "phc-toolkit/repeat-stx.rkt") untyped)]]

