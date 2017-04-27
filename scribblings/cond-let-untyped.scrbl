#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/cond-let]]
@(def-orig typed [phc-toolkit/cond-let])
@title{Untyped versions of cond-let}
@defmodule[phc-toolkit/untyped/cond-let
           #:use-sources
           [(submod (lib "phc-toolkit/cond-let.rkt") untyped)]]

