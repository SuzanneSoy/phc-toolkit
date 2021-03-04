#lang scribble/manual
@require[@for-label[phc-toolkit
                    racket/base]]

@title{phc-toolkit}
@author{@author+email["Suzanne Soy" "racket@suzanne.soy"]}

@defmodule[phc-toolkit]

This library contains a small toolkit of utilities used by
the @url{https://github.com/jsmaniac/phc} project and other
helper libraries for that project.

This library exports the following typed modules:

@itemlist[
 @item{@racketmodname[phc-toolkit/stx]}
 @item{@racketmodname[phc-toolkit/misc]}
 @item{@racketmodname[phc-toolkit/percent]}
 @item{@racketmodname[phc-toolkit/meta-struct]}
 @item{…}]

Untyped versions of the above modules are available under 
@racketmodname[phc-toolkit/untyped], which also contains the
following additional untyped-only modules:
@itemlist[
 @item{@racketmodname[phc-toolkit/untyped/for-star-list-star]}]

The @secref{template-lib} document discusses the
hypothetical features of a still-unwritten parser and
template library. The template part aims to be the pendant
of @racket[match] and @racket[syntax/parse], and the parser
part should unify @racket[match] and @racket[syntax/parse],
to enable parsing of syntax and regular data alike. This
library is not implemented yet, and will probably be moved
to a separate package when it is.

@(local-table-of-contents)

@include-section{aliases.scrbl}
@include-section{backtrace.scrbl}
@include-section{compat.scrbl}
@include-section{cond-let.scrbl}
@include-section{contract.scrbl}
@include-section{eval-get-values.scrbl}
@include-section{fixnum.scrbl}
@include-section{generate-indices.scrbl}
@include-section{ids.scrbl}
@include-section{in.scrbl}
@include-section{list-lang.scrbl}
@include-section{list.scrbl}
@include-section{logn-id.scrbl}
@include-section{misc.scrbl}
@include-section{multiassoc-syntax.scrbl}
@include-section{not-implemented-yet.scrbl}
@include-section{percent.scrbl}
@include-section{repeat-stx.scrbl}
@include-section{require-provide.scrbl}
@include-section{sequence.scrbl}
@include-section{set.scrbl}
@include-section{stx.scrbl}
@include-section{syntax-parse.scrbl}
@include-section{test-framework.scrbl}
@include-section{threading.scrbl}
@include-section{tmpl.scrbl}
@include-section{typed-rackunit.scrbl}
@include-section{typed-rackunit-extensions.scrbl}
@include-section{typed-untyped.scrbl}
@include-section{type-inference-helpers.scrbl}
@include-section{values.scrbl}
@include-section{untyped.scrbl}
@include-section{for-star-list-star.scrbl}
@include-section{meta-struct.scrbl}
@include-section{format-id-record-untyped.scrbl}

@include-section{template.scrbl}
