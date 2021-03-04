#lang scribble/manual
@require["utils.rkt"
         @for-label[phc-toolkit/typed-rackunit
                    racket/base
                    racket/list
                    (only-in racket compose ...)
                    racket/match
                    syntax/parse]]

@(def-orig orig [rackunit]
   check-equal?
   check-not-equal?
   check-true
   check-exn
   check-not-exn)

@(def-orig tr [typed/rackunit]
   check-equal?
   check-not-equal?
   check-true
   check-exn
   check-not-exn)

@title{Utilities and patches for @racketmodname[typed/rackunit]}
@author{@author+email["Suzanne Soy" "racket@suzanne.soy"]}

@defmodule[phc-toolkit/typed-rackunit]

@; TODO: add the message parameter when it is implemented
@defform[#:literals (:)
         (check-equal?: actual maybe-type expected)
         #:grammar [(actual (ExpressionOf Any))
                    (expected (ExpressionOf Any))
                    (maybe-type (code:line)
                                (code:line : type))
                    (type Type)]]{
 Typed macro which behaves like the @orig:check-equal? function. The official
 typed version @tr:check-equal? from @racketmodname[typed/rackunit] has some
 issues with source location for failed tests, and with higher-order values
 (e.g. syntax) passed as @racket[Any]. This alternate implementation fixes these
 issues.

 This implementation is compatible with the use of other
 functions from @racketmodname[typed/rackunit].
}

@defform[#:literals (:)
         (check-not-equal?: actual maybe-type expected)
         #:grammar [(actual (ExpressionOf Any))
                    (expected (ExpressionOf Any))
                    (maybe-type (code:line)
                                (code:line : type))
                    (type Type)]]{
 Typed macro which behaves like the @orig:check-not-equal? function. The
 official typed version @tr:check-not-equal? from
 @racketmodname[typed/rackunit] has some issues with source location for failed
 tests, and with higher-order values (e.g. syntax) passed as @racket[Any]. This
 alternate implementation fixes these issues.

 This implementation is compatible with the use of other
 functions from @racketmodname[typed/rackunit].
}

@defform[#:literals (:)
         (check-true: actual)
         #:grammar [(actual (ExpressionOf Any))]]{
 Typed macro which behaves like the @orig:check-true function. The official
 typed version @tr:check-true from @racketmodname[typed/rackunit] has some
 issues with source location for failed tests, and possibly with higher-order
 values (e.g. syntax) passed as @racket[Any]. This alternate implementation
 fixes these issues.

 This implementation is compatible with the use of other
 functions from @racketmodname[typed/rackunit].}

@defform[#:literals (:)
         (check-exn: exn-predicate-or-regexp thunk maybe-message)
         #:grammar [(exn-predicate-or-regexp
                     (ExpressionOf (U Regexp (→ Any Any))))
                    (thunk (→ Any))
                    (maybe-message (code:line)
                                   (code:line (ExpressionOf String)))]]{
 Typed macro which behaves like the @orig:check-exn function. The official
 typed version @tr:check-exn from @racketmodname[typed/rackunit] has some
 issues with source location for failed tests, and possibly with higher-order
 values (e.g. syntax) passed as @racket[Any]. This alternate implementation
 fixes these issues.

 This implementation is compatible with the use of other
 functions from @racketmodname[typed/rackunit].}

@defform[#:literals (:)
         (check-not-exn: thunk maybe-message)
         #:grammar [(thunk (→ Any))
                    (maybe-message (code:line)
                                   (code:line (ExpressionOf String)))]]{
 Typed macro which behaves like the @orig:check-not-exn function. The official
 typed version @tr:check-not-exn from @racketmodname[typed/rackunit] has some
 issues with source location for failed tests, and possibly with higher-order
 values (e.g. syntax) passed as @racket[Any]. This alternate implementation
 fixes these issues.

 This implementation is compatible with the use of other
 functions from @racketmodname[typed/rackunit].}

@include-section{typed-rackunit-untyped.scrbl}
