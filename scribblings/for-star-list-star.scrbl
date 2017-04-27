#lang scribble/manual
@require[@for-label[phc-toolkit/stx
                    racket/base]]

@title{for*/list*}
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}

@defmodule[phc-toolkit/untyped/for-star-list-star]

@defform[(for*/list* [sequences …] . body)
         #:grammar ([sequences
                     (* [id seq-expr] …)
                     ([id seq-expr] …)])]{
 This form allows iteration over sequences, collecting
 nested lists as the final result. Each @racket[sequences]
 group of @racket[[id seq-expr]] starts a new level of
 nesting. When the @racket[*] is present at the beginning of
 a group, its bindings are evaluated in sequence (like 
 @racket[let*] and @racket[for*/list]), otherwise they are
 evaluated in parallel (like @racket[let] and 
 @racket[for/list]).

 This form is equivalent to:
 @racketblock[
 (for/list ([id seq-expr …])
   (for/list ([id seq-expr …])
     (for/list ([id seq-expr …])
       …
         (for/list ([id seq-expr …])
           body))))]
 except when a group of @racket[[id seq-expr]] starts with
 a @racket[*], then @racket[for*/list] is used for that
 group instead of @racket[for/list].}