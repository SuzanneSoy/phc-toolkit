#lang typed/racket/base

(module m1 racket/base
  (require alexis/bool
           racket/function)

  (provide prefab-struct?)

  (define prefab-struct? (compose true? prefab-struct-key)))

(module m2 typed/racket/base
  (provide PrefabKey
           PrefabTop
           prefab-struct?
           make-prefab-struct
           ;; Imprecise type (m3 gives a more precise type to these):
           prefab-struct-key
           prefab-key?)

  (define-type PrefabKey (U Symbol
                            (List Symbol
                                  ; Optional: Nonnegative-Integer
                                  (List Nonnegative-Integer Any)
                                  (Vectorof Nonnegative-Integer))
                            (List Symbol
                                  Nonnegative-Integer ; Optional
                                  (List Nonnegative-Integer Any)
                                  (Vectorof Nonnegative-Integer))
                            (List* Symbol
                                   ; Optional: Nonnegative-Integer
                                   (List Nonnegative-Integer Any)
                                   (Vectorof Nonnegative-Integer)
                                   PrefabKey)
                            (List* Symbol
                                   Nonnegative-Integer ; Optional
                                   (List Nonnegative-Integer Any)
                                   (Vectorof Nonnegative-Integer)
                                   PrefabKey)))

  (require typed/racket/unsafe)
  (unsafe-require/typed (submod ".." m1) [#:opaque PrefabTop prefab-struct?])
  
  (require/typed racket [make-prefab-struct (→ PrefabKey Any * PrefabTop)])

  (require/typed racket
                 [prefab-struct-key (→ Any (U #f PrefabKey))]
                 [prefab-key? (→ Any Boolean)]))

(module m3 typed/racket/base
  (require typed/racket/unsafe)
  (require (except-in (submod ".." m2) prefab-struct-key prefab-key?))

  (provide prefab-struct-key prefab-key?)
  
  ;; Give a more precise type, while still ensuring that at least part of it
  ;; is validated by a contract:
  (unsafe-require/typed (submod ".." m2)
                        [prefab-struct-key (case→ (→ PrefabTop PrefabKey)
                                                  (→ Any #f))]
                        [prefab-key? (→ Any Boolean : PrefabKey)]))

(require (except-in 'm2 prefab-struct-key prefab-key?)
         'm3)

(provide PrefabKey
         PrefabTop
         prefab-struct?
         make-prefab-struct
         prefab-struct-key
         prefab-key?)