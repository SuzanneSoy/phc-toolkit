#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/syntax-parse
                    racket/base
                    syntax/parse]]

@title{@racket[syntax-parse] helpers}
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}

@defmodule[phc-toolkit/syntax-parse
           #:use-sources
           [(submod (lib "phc-toolkit/syntax-parse.rkt") typed)]]

@defidform[stx]{
 This identifier can only be used in the body of some forms,
 like @racket[define-syntax]. It is an error to use it as an
 expression elsewhere.}

@defform[(define-syntax/case (name . args) (literal-id ...) . body)]{
 This form is roughly equivalent to:
 
 @racketblock[(define-syntax (name stx)
                (syntax-case stx (literal-id ...)
                  [(_ . args) (let () . body)]))]
 
 Within @racket[body], the syntax parameter @racket[stx] can be used to refer to
 the whole syntax given as an argument to @racket[name].}


@(define ntax-patterns (tech #:doc '(lib "syntax/scribblings/syntax.scrbl")
                             #:key "syntax pattern"
                             "syntax-patterns"))
@(define ttern-directive (tech #:doc '(lib "syntax/scribblings/syntax.scrbl")
                               #:key "pattern-directive"
                               "pattern-directive"))

@(define tterns
   @seclink["stx-patterns"
            #:doc '(lib "scribblings/reference/reference.scrbl")]{patterns})

@(define ttern
   @seclink["stx-patterns"
            #:doc '(lib "scribblings/reference/reference.scrbl")]{pattern})

@defform[(define-syntax/parse (name . #,ntax-patterns)
           #,ttern-directive ... . body)]{
 This form is roughly equivalent to:

 @racketblock[(define-syntax (name stx)
                (syntax-parse stx
                  [(_ . #,ntax-patterns) #,ttern-directive ... . body]))]
 
 Within the @racket[#,ntax-patterns], the @racket[#,ttern-directive] and the
 @racket[body], the syntax parameter @racket[stx] can be used to refer to the
 whole syntax given as an argument to @racket[name].}

@defform[(λ/syntax-parse (name . #,ntax-patterns)
           #,ttern-directive ... . body)]{
 This form is roughly equivalent to:

 @racketblock[(λ (stx)
                (syntax-parse stx
                  [(_ . #,ntax-patterns) #,ttern-directive ... . body]))]
 
 Within the @racket[#,ntax-patterns], the @racket[#,ttern-directive] and the
 @racket[body], the syntax parameter @racket[stx] can be used to refer to the
 whole syntax given as an argument to the function.}

@defform[(define-for-syntax/case-args (name (pattern ...)) . body)]{
 This form is roughly equivalent to:

 @racketblock[(define-for-syntax (name _arg ...)
                (with-syntax ([pattern _arg] ...)
                  . body))]

 where each @racket[_arg] is a fresh identifier.}


@defform[(λ/syntax-case #,tterns (literal ...) . body)]{
 This form is roughly equivalent to:

 @racketblock[(λ (stx)
                (syntax-case stx (literal ...)
                  [(_ . #,tterns) ... . body]))]
 
 Within the @racket[#,tterns], and the @racket[body], the syntax parameter
 @racket[stx] can be used to refer to the whole syntax given as an argument to
 the function.}

@defform[(define/case-args (name (#,ttern ...)) . body)]{
 This form is roughly equivalent to:

 @racketblock[(define (name _arg ...)
                (with-syntax ([#,ttern _arg] ...)
                  . body))]

 where each @racket[_arg] is a fresh identifier.}

@include-section{syntax-parse-pattern-expanders.scrbl}
@include-section{syntax-parse-untyped.scrbl}