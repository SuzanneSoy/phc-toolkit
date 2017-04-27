#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/untyped/typed-rackunit]]
@(def-orig typed [phc-toolkit/typed-rackunit])
@title{Untyped versions of typed-rackunit}
@defmodule[phc-toolkit/untyped/typed-rackunit
           #:use-sources
           [(lib "phc-toolkit/typed-rackunit.rkt")]]

