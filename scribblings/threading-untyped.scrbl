#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/threading]]
@(def-orig typed [phc-toolkit/threading])
@title{Untyped versions of threading}
@defmodule[phc-toolkit/untyped/threading
           #:use-sources
           [(submod (lib "phc-toolkit/threading.rkt") untyped)]]

