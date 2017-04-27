#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/syntax-parse]]

@(def-orig orig [phc-toolkit/syntax-parse]
   stx
   define-syntax/case
   define-syntax/parse)

@title{Untyped versions of @racket[syntax-parse] helpers}

@defmodule[phc-toolkit/untyped/syntax-parse
           #:use-sources
           [(submod (lib "phc-toolkit/syntax-parse.rkt") untyped)]]

@defidform[stx]{
 Untyped version of @|orig:stx|.
}

@defidform[define-syntax/case]{Untyped version of @|orig:define-syntax/case|.}
@defidform[define-syntax/parse]{Untyped version of @|orig:define-syntax/parse|.}