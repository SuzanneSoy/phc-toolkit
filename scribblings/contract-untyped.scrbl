#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/contract]]
@(def-orig typed [phc-toolkit/contract])
@title{Untyped versions of contract}
@defmodule[phc-toolkit/untyped/contract
           #:use-sources
           [(submod (lib "phc-toolkit/contract.rkt") untyped)]]

