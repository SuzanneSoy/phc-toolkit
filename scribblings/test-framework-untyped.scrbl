#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/test-framework]]
@(def-orig typed [phc-toolkit/test-framework])
@title{Untyped versions of test-framework}
@defmodule[phc-toolkit/untyped/test-framework
           #:use-sources
           [(submod (lib "phc-toolkit/test-framework.rkt") untyped)]]

