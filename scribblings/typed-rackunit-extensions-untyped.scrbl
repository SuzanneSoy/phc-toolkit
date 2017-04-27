#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/typed-rackunit-extensions]]
@(def-orig typed [phc-toolkit/typed-rackunit-extensions])
@title{Untyped versions of typed-rackunit-extensions}
@defmodule[phc-toolkit/untyped/typed-rackunit-extensions
           #:use-sources
           [(submod (lib "phc-toolkit/typed-rackunit-extensions.rkt") untyped)]]

