#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/backtrace]]
@(def-orig typed [phc-toolkit/backtrace])
@title{Untyped versions of backtrace}
@author{@author+email["Suzanne Soy" "racket@suzanne.soy"]}
@defmodule[phc-toolkit/untyped/backtrace
           #:use-sources
           [(submod (lib "phc-toolkit/backtrace.rkt") untyped)]]

