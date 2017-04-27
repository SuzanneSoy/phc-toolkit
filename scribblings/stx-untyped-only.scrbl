#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/stx]]

@title{Transformers utilities}

@(declare-exporting phc-toolkit/stx
                    #:use-sources
                    [(lib "phc-toolkit/untyped-only/stx.rkt")])

@defproc[(make-rest-transformer [tranformer-function (-> syntax? syntax?)])
         (-> syntax? syntax?)]{
 Returns a transformer function which applies @racket[tranformer-function] on
 the @racket[stx-cdr] of its argument. It is a shorthand for:

 @racketblock[(λ (stx)
                (syntax-case stx ()
                  [(_ . rest) (f #'rest)]))]
}

@defproc[(make-id+call-transformer [result syntax?])
         (-> syntax? syntax?)]{
 Returns a transformer function which returns:
 @itemlist[
 @item{the given @racket[result], when it is called as an identifier macro}
 @item{@racket[(result arg ...)] where the @racket[arg ...] are the macro's
   arguments (except the macro identifier itself), when it is called as a
   regular macro.}]

 It is a shorthand for:

 @RACKETBLOCK[(λ (stx)
                (syntax-case stx ()
                  [(_ . args) (quasisyntax/top-loc stx (#,result . args))]
                  [id (identifier? #'id) result]))]
}

@defproc[(make-id+call-transformer-delayed [result (-> syntax?)])
         (-> syntax? syntax?)]{
                               
 Like @racket[make-id+call-transformer], but the result is wrapped in a
 function which is evaluated only when the returned transformer function is
 run. This is useful when the expression depends on some mutable context.}