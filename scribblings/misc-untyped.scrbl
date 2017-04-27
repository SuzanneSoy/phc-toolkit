#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/misc]]
@(def-orig typed [phc-toolkit/misc])
@title{Untyped versions of misc}
@defmodule[phc-toolkit/untyped/misc
           #:use-sources
           [(submod (lib "phc-toolkit/misc.rkt") untyped)]]

