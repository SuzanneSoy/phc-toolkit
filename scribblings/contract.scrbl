#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/contract
                    racket/function
                    racket/contract]]
@title{contract}
@author{@author+email["Suzanne Soy" "racket@suzanne.soy"]}
@defmodule[phc-toolkit/contract
           #:use-sources
           [(submod (lib "phc-toolkit/contract.rkt") typed)]]

@defproc[(regexp-match/c [rx (or/c string? regexp?)]) contract?
         #:value (and/c (or/c string? bytes? path? input-port?)
                        (curry regexp-match? rx))]{
                                                       
 Returns a contract which accepts only values matching the given regular
 expression.}

@defproc[(id/c [id identifier?]) contract?
         #:value (and/c identifier? (curry free-identifier=? id))]{
 Returns a contract which accepts only identifiers which are
 @racket[free-identifier=?] to @racket[id].}

@defidform[define/contract?]{
 Like @racket[define/contract], but later versions of this library may allow
 disabling the contracts via a parameter or syntax parameter. This form will be
 useful for internal functions, to ease debugging during development, but with
 the (future) possibility of disabling the contracts in the final version, to
 avoid the performance cost of checking many contracts between internal
 functions.}

@include-section{contract-untyped.scrbl}
