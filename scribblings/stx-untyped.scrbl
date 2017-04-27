#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/stx]]

@(def-orig typed [phc-toolkit/stx]
   stx-assoc
   identifier->string
   identifier→string
   make-rest-transformer
   make-id+call-transformer
   quasisyntax/top-loc
   syntax/top-loc
   quasisyntax/whole-loc
   syntax/whole-loc)

@title{Untyped versions of syntax object manipulation utilities}

@defmodule[phc-toolkit/untyped/stx
           #:use-sources
           [(submod (lib "phc-toolkit/stx.rkt") untyped)
            (lib "phc-toolkit/stx/fold.rkt")]]

@defidform[stx-assoc]{Untyped version of @|typed:stx-assoc|.}

@defproc*[([(identifier->string [identifier Identifier]) String]
           [(identifier→string [identifier Identifier]) String])]{
 Untyped version of @|typed:identifier->string| and @|typed:identifier→string|.
}

@defidform[make-rest-transformer]{
 Untyped version of @|typed:make-rest-transformer|.}

@defidform[make-id+call-transformer]{
 Untyped version of @|typed:make-id+call-transformer|.}

@defidform[quasisyntax/top-loc]{
 Untyped version of @|typed:quasisyntax/top-loc|.}

@defidform[syntax/top-loc]{
 Untyped version of @|typed:syntax/top-loc|.}

@defidform[quasisyntax/whole-loc]{
 Untyped version of @|typed:quasisyntax/whole-loc|.}

@defidform[syntax/whole-loc]{
 Untyped version of @|typed:syntax/whole-loc|.}
