#lang typed/racket/base (require phc-toolkit/is-typed)
(provide (all-from-out racket/match)
         ∘
         …
         …+
         attr
         when-attr
         @
         match-λ
         match-λ*
         match-λ**
         generate-temporary
         true?
         false?)
  
(require racket/match)
  
(require (only-in racket/base
                  [compose ∘]
                  [... …])
         (only-in racket/bool
                  false?)
         (only-in syntax/parse
                  [...+ …+])
         (only-in phc-toolkit/untyped-only/syntax-parse
                  [attribute* attr]
                  [attribute* @]))

(define-syntax-rule (when-attr a e)
  (if (attr a) e #'()))
  
(require (only-in racket/match
                  [match-lambda match-λ]
                  [match-lambda* match-λ*]
                  [match-lambda** match-λ**]))
  
(require/typed racket/syntax [generate-temporary (→ Any Identifier)])

(if-typed
 (require (only-in alexis/bool true?))
 (require (only-in typed/alexis/bool true?)))