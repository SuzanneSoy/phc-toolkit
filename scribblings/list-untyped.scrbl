#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/list]]
@(def-orig typed [phc-toolkit/list])
@title{Untyped versions of list}
@defmodule[phc-toolkit/untyped/list
           #:use-sources
           [(submod (lib "phc-toolkit/list.rkt") untyped)]]

