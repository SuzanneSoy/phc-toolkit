#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/tmpl-multiassoc-syntax]]
@title{Template metafunction for @racket[multiassoc-syntax]}
@defmodule[phc-toolkit/tmpl-multiassoc-syntax
           #:use-sources
           [(submod (lib "phc-toolkit/tmpl-multiassoc-syntax.rkt")
                    typed
                    m-tmpl-cdr-assoc-syntax)]]

@deftogether[
 [@defform[#:kind "template metafunction"
           (tmpl-cdr-assoc-syntax maybe-default query [k . v] …)
           #:grammar
           [(maybe-default (code:line)
                           (code:line #:default default))]]
  @defform[#:kind "template metafunction"
           (!cdr-assoc maybe-default query [k . v] …)
           #:grammar
           [(maybe-default (code:line)
                           (code:line #:default default))]]]]{
                                                          
 This template metafunction returns the first @racket[v] whose @racket[k] is
 @racket[free-identifier=?] to the given @racket[query]. If no such @racket[k]
 exists, then @racket[default] is returned if specified, and otherwise an error
 is raised while expanding the template.}

