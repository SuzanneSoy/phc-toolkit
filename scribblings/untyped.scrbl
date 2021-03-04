#lang scribble/manual
@require[@for-label[phc-toolkit/stx
                    racket/base]]

@title{Untyped versions of the modules}
@author{@author+email["Suzanne Soy" "racket@suzanne.soy"]}

@defmodule[phc-toolkit/untyped]

The module @racketmodname[phc-toolkit/untyped] and the
modules below it (@racketmodname[phc-toolkit/untyped/stx]
@etc) provide the same bindings as 
@racketmodname[phc-toolkit], but those functions and macros
are declared in an untyped context. This means that no check
is performed on the arguments (contracts have not been added
yet to the definitions in this library). This untyped
version of the library exists mainly so that macros defined
within the modules work with untyped @racketmodname[racket],
as macros declared within a @racket[typed/racket] module
normally throw an error when used in an untyped context.

The following untyped modules are available (a link to the
typed version is noted for each):

@(define-syntax-rule (u untyped typed)
   @item{@racketmodname[untyped] (@racketmodname[typed])})

@itemlist[
 @u[phc-toolkit/untyped/aliases phc-toolkit/aliases]
 @u[phc-toolkit/untyped/cond-let phc-toolkit/cond-let]
 @u[phc-toolkit/untyped/fixnum phc-toolkit/fixnum]
 @u[phc-toolkit/untyped/generate-indices phc-toolkit/generate-indices]
 @u[phc-toolkit/untyped/ids phc-toolkit/ids]
 @u[phc-toolkit/untyped/list phc-toolkit/list]
 @u[phc-toolkit/untyped/logn-id phc-toolkit/logn-id]
 @u[phc-toolkit/untyped/misc phc-toolkit/misc]
 @u[phc-toolkit/untyped/multiassoc-syntax phc-toolkit/multiassoc-syntax]
 @u[phc-toolkit/untyped/not-implemented-yet phc-toolkit/not-implemented-yet]
 @u[phc-toolkit/untyped/percent phc-toolkit/percent]
 @u[phc-toolkit/untyped/repeat-stx phc-toolkit/repeat-stx]
 @u[phc-toolkit/untyped/require-provide phc-toolkit/require-provide]
 @u[phc-toolkit/untyped/sequence phc-toolkit/sequence]
 @u[phc-toolkit/untyped/set phc-toolkit/set]
 @u[phc-toolkit/untyped/stx phc-toolkit/stx]
 @u[phc-toolkit/untyped/syntax-parse phc-toolkit/syntax-parse]
 @u[phc-toolkit/untyped/threading phc-toolkit/threading]
 @u[phc-toolkit/untyped/tmpl-multiassoc-syntax
    phc-toolkit/tmpl-multiassoc-syntax]
 @u[phc-toolkit/untyped/tmpl phc-toolkit/tmpl]
 @u[phc-toolkit/untyped/typed-rackunit-extensions
    phc-toolkit/typed-rackunit-extensions]
 @u[phc-toolkit/untyped/typed-rackunit phc-toolkit/typed-rackunit]
 @u[phc-toolkit/untyped/type-inference-helpers
    phc-toolkit/type-inference-helpers]
 @u[phc-toolkit/untyped/values phc-toolkit/values]
 @u[phc-toolkit/untyped/meta-struct phc-toolkit/meta-struct]]

Furthermore, the following module is only available as an
untyped module:

@itemlist[
 @item{@racketmodname[phc-toolkit/untyped/for-star-list-star]}]
