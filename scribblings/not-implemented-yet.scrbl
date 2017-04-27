#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/not-implemented-yet]]
@title{not-implemented-yet}
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}
@defmodule[phc-toolkit/not-implemented-yet
           #:use-sources
           [(submod (lib "phc-toolkit/not-implemented-yet.rkt") typed)]]

@defform[(? type expr ...)]{
 Can be used as a placeholder for an expression returning @racket[type]. This
 form throws an error at run-time, but will allow the program to typecheck so
 that the developer can focus on other parts without a myriad of type errors,
 and can come back to implement the @racket[?] placeholders later.

 The @racket[expr ...] expressions are included within a @racket[lambda]
 function, after the @racket[(error "Not implemented yet")], so Typed/Racket's
 dead code detection will ignore most type errors within those expressions.
 This makes @racket[?] useful as a joker to temporarily ignore type errors
 within the expressions, while annotating them with the type they should
 normally have once they are fixed.}

@defform[(?* expr ...)]{

 Can be used as a placeholder for an expression returning @racket[Nothing].
 This form throws an error at run-time, but will allow the program to typecheck
 so that the developer can focus on other parts without a myriad of type
 errors, and can come back to implement the expressions marked with @racket[?*]
 later.
 
 The @racket[expr ...] expressions are included within a @racket[lambda]
 function, after the @racket[(error "Not implemented yet")], so Typed/Racket's
 dead code detection will ignore most type errors within those expressions.
 This makes @racket[?*] useful as a joker to temporarily ignore type errors
 within the expressions. @racket[?*] is also useful as a joker to allow the
 whole @racket[(?* expr ...)] expression to be used as an argument to nearly
 any function, as it has the type @racket[Nothing], i.e. "bottom", which is a
 subtype of (nearly) all other types (no value has the type @racket[Nothing],
 i.e. it is the return type of functions which never return, which is the case
 here, since @racket[?*] always throws an error at run-time.

 Caveat: the @racket[Nothing] type can propagate (when Typed/Racket encounters
 a function called with @racket[Nothing] as the type of one of its arguments,
 it may mark the return value of that function as @racket[Nothing] too, since
 the call may never happen). This means that other parts of the code may be
 considered dead code, and type errors in these other parts may be ignored.}

@include-section{not-implemented-yet-untyped.scrbl}
