#lang scribble/manual
@require[racket/require
         "utils.rkt"
         @for-label[phc-toolkit/stx]]

@(def-orig typed [phc-toolkit/stx]
   stx-assoc
   identifier->string
   identifier→string
   quasisyntax/top-loc
   syntax/top-loc
   quasisyntax/whole-loc
   syntax/whole-loc)

@title{Patching source locations}

@(declare-exporting phc-toolkit/stx
                    #:use-sources
                    [(lib "phc-toolkit/stx/fold.rkt")])

@defform[(quasisyntax/top-loc stx-expr quasitemplate)]{
 Like @racket[(quasisyntax/loc stx-expr quasitemplate)], but the source
 location for all "top" parts of the resulting syntax object are updated, so
 long as their source location is the same as the source location for the
 topmost part of the @racket[quasitemplate].

 In other words, this does a traversal of the syntax object and updates the
 source location of the traversed parts, but the traversal does not go within a
 part whose source file differs from that of the @racket[quasitemplate].

 For example, in the following code, the source location of parts within
 @racket[user-code] will not be updated (unless @racket[user-code] originates
 from the same file as @racket[quasitemplate]), but the source location of all
 other parts will be updated, including the @racket[begin] identifier and its
 surrounding form (its surrounding "pair of parentheses"). In contrast,
 @racket[quasisyntax/loc] would have updated only the topmost syntax object,
 i.e. the outermost "pair of parentheses" of the @racket[let] form.

 @racketblock[(λ (stx)
                (syntax-case stx ()
                  [(_ . user-code)
                   (with-syntax ([bg #'(begin . user-code)])
                     (quasisyntax/top-loc stx (let () bg)))]))]}

@defform[(syntax/top-loc stx-expr quasitemplate)]{
 Like @racket[(syntax/loc stx-expr quasitemplate)], but the source location
 for all "top" parts of the resulting syntax object are updated, like is done
 by @racket[quasisyntax/top-loc].}


@defform[(quasisyntax/whole-loc stx-expr quasitemplate)]{
                                                         
 Like @racket[(quasisyntax/top-loc stx-expr quasitemplate)], but the source
 location for all parts of the resulting syntax object are updated if they
 belong to the same source file as the @racket[quasitemplate], not only the
 "top" ones.

 In the following example, all parts of the syntax object which source file is
 the same as the macro will be updated, including those within
 @racket[user-code] (e.g. if the @racket[user-code] contains code generated by
 other macros from the same file.

 @racketblock[(λ (stx)
                (syntax-case stx ()
                  [(_ . user-code)
                   (with-syntax ([bg #'(begin . user-code)])
                     (quasisyntax/whole-loc stx (let () bg)))]))]

 This is usually not needed, as @racket[quasisyntax/top-loc] would have
 updated the source location of @racket[1], @racket[2] and @racket[3] and their
 surrounding syntax list (the "pair of parentheses" around them), since their
 surrounding syntax list comes from the same file as the macro:

 @racketblock[(λ (stx)
                (syntax-case stx ()
                  [(_ . user-function)
                   (quasisyntax/top-loc stx
                     (user-function 1 2 3))]))]}

@defform[(syntax/whole-loc stx-expr quasitemplate)]{
 Like @racket[(syntax/top-loc stx-expr quasitemplate)], but the source
 location for all parts of the resulting syntax object are updated if they
 belong to the same source file as the @racket[quasitemplate], not only the
 "top" ones, like is done by @racket[quasisyntax/whole-loc].}
