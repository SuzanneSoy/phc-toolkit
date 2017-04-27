#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/not-implemented-yet]]
@(def-orig typed [phc-toolkit/not-implemented-yet])
@title{Untyped versions of not-implemented-yet}
@defmodule[phc-toolkit/untyped/not-implemented-yet
           #:use-sources
           [(submod (lib "phc-toolkit/not-implemented-yet.rkt") untyped)]]

