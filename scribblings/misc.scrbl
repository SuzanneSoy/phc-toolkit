#lang scribble/manual

@(require (for-label typed/racket/base
                     phc-toolkit/misc))

@(module racket-ids racket/base
   (require scribble/manual
            (for-label predicates))

   (define or?-id (racket or?))
   (provide (all-defined-out)))

@(require 'racket-ids)

@title{Miscellaneous utilities}

@section{Typed miscellaneous utilities}

@defmodule[phc-toolkit/misc]

@defproc[(hash-set** [h (HashTable K V)] [l* (Listof (Pairof K V))])
         (HashTable K V)]{
 Calls @racket[hash-set] on the hash @racket[h] for each
 key-value pair contained in each list of @racket[l*].}

@defform[(with-output-file [var filename] maybe-mode maybe-exists body …)
         #:grammar ([var Identifier]
                    [filename (ExpressionOf String)]
                    [maybe-mode (code:line) (code:line #:mode mode)]
                    [maybe-exists (code:line) (code:line #:exists exists)])]{
 Executes body with @racket[var] bound to the 
 @racket[output-port?] obtained when opening the file. The
 port is automatically closed at the end of the 
 @racket[body]. This is a macro version of 
 @racket[call-with-output-file].}

@defproc[(or? [f (→ A Boolean)] ...) (→ A (U A #f))]{
 Typed version of @or?-id from the 
 @racketmodname[predicates] package, which returns the value
 itself when all predicates are satisfied instead of just
 returning @racket[#t].}

@subsection{Untyped versions of miscellaneous utilities}

@defmodule[phc-toolkit/untyped #:link-target? #f]

@defproc[(hash-set** [h (HashTable K V)] [l* (Listof (Pairof K V))])
         (HashTable K V)]{
 Untyped version.}

@defform[(with-output-file [var filename] maybe-mode maybe-exists body …)
         #:grammar ([var Identifier]
                    [filename (ExpressionOf String)]
                    [maybe-mode (code:line) (code:line #:mode mode)]
                    [maybe-exists (code:line) (code:line #:exists exists)])]{
 Untyped version.}


@defproc[(or? [f (→ A Boolean)] ...) (→ A (U A #f))]{
 Untyped version.}

@include-section{misc-untyped.scrbl}
