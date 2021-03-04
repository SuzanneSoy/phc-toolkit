#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/syntax-parse
                    racket/base
                    syntax/parse]]

@(def-orig orig [syntax/parse]
   ~or
   ~literal
   ~parse
   ~bind)

@title{Pattern expanders}
@author{@author+email["Suzanne Soy" "racket@suzanne.soy"]}

@declare-exporting[phc-toolkit/syntax-parse
                   #:use-sources
                   [(submod (lib "phc-toolkit/syntax-parse.rkt") typed)]]

@defform[#:kind "pattern expander"
         (~either alt ...)]{
 Like @orig:~or, but with no special behaviour when present under ellipses.
 The use case for this is that @racket[({~or {~and 1 x} {~and 2 x}} ...)] would
 match any list of @racket[1]s and @racket[2]s in any order, but it complains
 that the attribute is bound twice, since both alternatives within the
 @racket[~or] are understood as separate patterns, not mutually-exclusive
 choices. On the other hand @racket[({~either {~and 1 x} {~and 2 x}} ...)] still
 matches @racket[(2 1 1 1 2 2 1)], and successfully binds all the elements to
 @racket[x ...].}

@defform[#:kind "pattern expander"
         (~lit alt ...)]{
 Alias for @|orig:~literal|.}

@defform[#:kind "pattern expander"
         (~with pat val)]{
 Alias for @|orig:~parse|, can be used semantically when @racket[#:with] would
 have been used in a syntax class definition.}

@defform[#:kind "pattern expander"
         (~attr attr-name val)]{
 Alias for @racket[(#,orig:~bind [attr-name val])], can be used semantically
 when @racket[#:attr] would have been used in a syntax class definition.}

@(define ttern
   @seclink["stxparse-patterns"
            #:doc '(lib "syntax/scribblings/syntax.scrbl")]{pattern})

@defform[#:kind "pattern expander"
         (~optkw kw #,ttern ...)
         #:contracts
         [(kw keyword?)]]{
 A shorthand for:

 @racketblock[{~optional {~seq {~and _name kw} #,ttern ...}}]

 where @racket[_name] is derived from the keyword, so that
 @racket[~optkw #:foo] binds the pattern variable @racket[foo].}


@defform[#:kind "pattern expander"
         (~optkw… kw #,ttern ...)
         #:contracts
         [(kw keyword?)]]{
 A shorthand for:
 
 @racketblock[(~optional {~seq {~and _name kw} #,ttern ...}
                         #:name "the kw keyword")]

 where the occurrence of @racket["kw"] within the string is replaced by the
 actual @racket[kw] keywords, and where the @racket[_name] is derived from the
 keyword, so that @racket[~optkw #:foo] binds the pattern variable
 @racket[foo], and uses the name @racket["the #:foo keyword"].

 This form can only be used where an
 @tech[#:doc '(lib "syntax/scribblings/syntax.scrbl")]{ellipsis-head pattern}
 is allowed.}


@defform[#:kind "pattern expander"
         (~maybe #,ttern ...)]{
 A shorthand for:
 
 @racketblock[(~optional {~seq #,ttern ...})]}


