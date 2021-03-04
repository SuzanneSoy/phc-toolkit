#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/backtrace]]
@title{backtrace}
@author{@author+email["Suzanne Soy" "racket@suzanne.soy"]}
@defmodule[phc-toolkit/backtrace
           #:use-sources
           [(submod (lib "phc-toolkit/backtrace.rkt") typed)]]

@include-section{backtrace-untyped.scrbl}
