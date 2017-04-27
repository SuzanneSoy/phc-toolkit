#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/ids]]
@(def-orig typed [phc-toolkit/ids])
@title{Untyped versions of ids}
@defmodule[phc-toolkit/untyped/ids
           #:use-sources
           [(submod (lib "phc-toolkit/ids.rkt") untyped)]]

