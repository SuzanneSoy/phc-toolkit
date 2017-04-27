#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/fixnum]]
@(def-orig typed [phc-toolkit/fixnum])
@title{Untyped versions of fixnum}
@defmodule[phc-toolkit/untyped/fixnum
           #:use-sources
           [(submod (lib "phc-toolkit/fixnum.rkt") untyped)]]

