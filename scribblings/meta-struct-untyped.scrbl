#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/meta-struct]]
@(def-orig typed [phc-toolkit/meta-struct]
   struct-predicate
   struct-constructor
   struct-accessor
   struct-type-is-immutable?
   struct-instance-is-immutable?)
@title{Untyped versions of the meta-struct typed macros}
@defmodule[phc-toolkit/untyped/meta-struct
           #:link-target? #f
           #:use-sources
           [(submod (lib "phc-toolkit/meta-struct.rkt") untyped)]]

@defidform[struct-predicate]{Untyped version of @|typed:struct-predicate|.}
@defidform[struct-constructor]{Untyped version of @|typed:struct-constructor|.}
@defidform[struct-accessor]{Untyped version of @|typed:struct-accessor|.}
@defidform[struct-type-is-immutable?]{
 Untyped version of @|typed:struct-type-is-immutable?|.}
@defidform[struct-instance-is-immutable?]{
 Untyped version of @|typed:struct-instance-is-immutable?|.}