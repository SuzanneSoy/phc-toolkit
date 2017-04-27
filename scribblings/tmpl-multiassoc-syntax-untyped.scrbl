#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/tmpl-multiassoc-syntax]]
@(def-orig typed [phc-toolkit/tmpl-multiassoc-syntax])
@title{Untyped versions of tmpl-multiassoc-syntax}
@defmodule[phc-toolkit/untyped/tmpl-multiassoc-syntax
           #:use-sources
           [(submod (lib "phc-toolkit/tmpl-multiassoc-syntax.rkt") untyped)]]

