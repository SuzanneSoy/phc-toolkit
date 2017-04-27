#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/tmpl]]
@(def-orig typed [phc-toolkit/tmpl])
@title{Untyped versions of tmpl}
@defmodule[phc-toolkit/untyped/tmpl
           #:use-sources
           [(submod (lib "phc-toolkit/tmpl.rkt") untyped)]]

