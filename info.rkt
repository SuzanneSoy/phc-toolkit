#lang info
(define collection "phc-toolkit")
(define deps '("base"
               "rackunit-lib"
               "alexis-util"
               "typed-racket-lib"
               "typed-racket-more"
               "reprovide-lang"
               "type-expander"
               "hyper-literate"
               "version-case"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "typed-racket-doc"
                     "predicates"
		     "rackunit-doc"
                     "scribble-math"
                     "drracket"))
(define scribblings '(("scribblings/phc-toolkit.scrbl" (multi-page))))
(define pkg-desc "My toolkit")
(define version "1.1")
(define pkg-authors '(|Suzanne Soy|))
