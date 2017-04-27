#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/backtrace]]
@title{backtrace}
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}
@defmodule[phc-toolkit/backtrace
           #:use-sources
           [(submod (lib "phc-toolkit/backtrace.rkt") typed)]]

@include-section{backtrace-untyped.scrbl}
