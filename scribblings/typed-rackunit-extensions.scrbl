#lang scribble/manual
@require[scribble-math
         "utils.rkt"
         @for-label[phc-toolkit/typed-rackunit-extensions]]
@title{Extensions for @racketmodname[typed/rackunit]}
@author{@author+email["Georges Dupéron" "georges.duperon@gmail.com"]}

@defmodule[phc-toolkit/typed-rackunit-extensions
           #:use-sources
           [(submod (lib "phc-toolkit/typed-rackunit-extensions.rkt") typed)]]

@defform[#:literals (:)
         (check-ann value type)
         #:grammar [(value (ExpressionOf type))
                    (type Type)]]{
 Verifies at compile-time that the given value is of the
 given type. The file will not compile if this check fails.
 
 TODO: do the check at run-time, like @racket[check-tc].}

@defform[(check-tc . body)]{
 Verifies at run-time that the statments in @racket[body]
 typecheck without any error.

 TODO: fix possible problems with source location when the
 test fails.}

@defform[(check-not-tc . body)]{
 Verifies at run-time that the statments in @racket[body]
 contain a type error. This can be used to check that the
 types provided by a library or generated by a macro are
 strong enough, by verifying that type errors that should be
 caught are caught.

 TODO: fix possible problems with source location when the
 test fails.}

@defproc[(check-equal?-classes [class (∀ (A) (Pairof String (Listof A)))] ...)
         Void]{
 Verivies that the given elements form equality classes as
 indicated.

 The @racket[car] of each class indicates its name, and the
 @racket[rest] is a list of element which belong to that
 class. All elements of the same class should have the same
 type @racket[Aᵢ], but elements of two different classes can
 have different types @racket[Aᵢ] and @racket[Aⱼ].

 This function checks that all elements of the same class
 are @racket[equal?], and that any two elements of two
 distinct classes are different. It also checks that
 elements are equal to themeselves, and checks equalities
 and inequalities in both directions, i.e. 
 @racket[(and (equal? a b) (equal? b a))] for equalities,
 and @racket[(and (not (equal? a b)) (not (equal? b a)))]
 for inequalities.

 Be aware that this function has @${O(n²)} time complexity,
 with @${n} being the total number of elements in all
 classes.}

@defform[#:literals (:)
         (check-equal?-classes: [maybe-nameᵢ maybe-typeᵢ elementᵢⱼ ...] ...)
         #:grammar [(maybe-nameᵢ (code:line)
                                 (code:line #:name String))
                    (maybe-typeᵢ (code:line)
                                 (code:line : tᵢ))
                    (tᵢ Type)
                    (elementᵢⱼ (ExpressionOf tᵢ or Any))]]{
 Macro form of @racket[check-equal?-classes]. It is
 equivalent to
 @racket[(check-equal?-classes
          (list nameᵢ elementᵢ ...) ...)], but also checks
 that each @racket[elementᵢⱼ] is of the corresponding 
 @racket[tᵢ] type, if specified.}

@include-section{typed-rackunit-extensions-untyped.scrbl}
