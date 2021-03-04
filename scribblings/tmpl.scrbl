#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/tmpl]]
@title{tmpl}
@author{@author+email["Suzanne Soy" "racket@suzanne.soy"]}
@defmodule[phc-toolkit/tmpl
           #:use-sources
           [(submod (lib "phc-toolkit/tmpl.rkt") typed)]]

@include-section{tmpl-untyped.scrbl}
