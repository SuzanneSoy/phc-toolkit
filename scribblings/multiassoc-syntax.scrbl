#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/multiassoc-syntax]]
@title{multiassoc-syntax}
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}
@defmodule[phc-toolkit/multiassoc-syntax
           #:use-sources
           [(submod (lib "phc-toolkit/multiassoc-syntax.rkt") typed)]]

@include-section{tmpl-multiassoc-syntax.scrbl}
@include-section{multiassoc-syntax-untyped.scrbl}
