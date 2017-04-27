#lang scribble/manual

@(require (for-label typed/racket/base
                     phc-toolkit/percent))

@title{@racket[let-in] binding and destructuring form}

@defmodule[phc-toolkit/percent]

The forms in this module may possibly be moved to a separate
package, as part of the template library described in 
@secref{template-lib} (for now the template library is not
implemented yet).

@defform[#:literals (in = and)
         (% parallel-binding …
            maybe-in
            body …)
         #:grammar
         [(parallel-binding (code:line binding and parallel-binding)
                            binding)
          (binding (code:line pattern … = expr))
          (maybe-in (code:line)
                    in)
          (expr expression)]]{
 Locally binds the variables in the @racket[pattern]s to the
 @racket[expr]. Each binding clause should contain as many 
 @racket[pattern]s as @racket[expr] produces values. The 
 @racket[body …] forms are evaluated with the given
 variables bound.

 The bindings are executed in sequence, as if bound with 
 @racket[let*], unless grouped using @racket[and], in which
 case they  are executed in parallel, as if bound with 
 @racket[let].

 NOTE: TODO: for now bindings are run in sequence, and
 parallel bindings have not been implemented yet.}

@defidform[in]{
 This identifier is only valid in certain forms, like 
 @racket[(% x = 10 in (+ x x))]. It is an error to use it as
 an expression otherwise.}

@defform[#:literals (: :: …)
         (define% (name pattern …)
           body …)
         #:grammar
         [(pattern variable
                   [variable : type]
                   cons-pattern
                   list-pattern
                   vector-pattern)
          (cons-pattern (pattern . pattern)
                        (pattern :: pattern))
          (list-pattern (pattern …)
                        (pattern … :: tail-pattern))
          (tail-pattern pattern)
          (vector-pattern #(pattern …))
          (variable identifier)]]{
 Locally binds the variables in the @racket[pattern]s to the
 @racket[expr]. Each binding clause should contain as many 
 @racket[pattern]s as @racket[expr] produces values. The 
 @racket[body …] forms are evaluated with the given
 variables bound.

 The bindings are executed in parallel, as if bound with 
 @racket[let].}

@include-section{percent-untyped.scrbl}
