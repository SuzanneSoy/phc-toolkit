#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/sequence]]
@(def-orig typed [phc-toolkit/sequence])
@title{Untyped versions of sequence}
@defmodule[phc-toolkit/untyped/sequence
           #:use-sources
           [(submod (lib "phc-toolkit/sequence.rkt") untyped)]]

