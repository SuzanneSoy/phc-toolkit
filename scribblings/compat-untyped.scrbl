#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/compat]]
@(def-orig typed [phc-toolkit/compat]
   with-disappeared-uses*
   record-disappeared-uses*)
@title{Untyped versions of compat}
@defmodule[phc-toolkit/untyped/compat
           #:use-sources
           [(submod (lib "phc-toolkit/compat.rkt") untyped)]]

@defidform[record-disappeared-uses*]{
 Untyped version of @|typed:record-disappeared-uses*|.}
@defidform[with-disappeared-uses*]{
 Untyped version of @|typed:with-disappeared-uses*|.}
