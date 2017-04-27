#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/multiassoc-syntax]]
@(def-orig typed [phc-toolkit/multiassoc-syntax])
@title{Untyped versions of multiassoc-syntax}
@defmodule[phc-toolkit/untyped/multiassoc-syntax
           #:use-sources
           [(submod (lib "phc-toolkit/multiassoc-syntax.rkt") untyped)]]

@include-section{tmpl-multiassoc-syntax-untyped.scrbl}