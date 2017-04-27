#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (provide check-equal?-classes
           check-equal?-classes:
           check-tc
           check-not-tc
           check-ann
           (for-syntax eval-tc))
  
  (require "typed-untyped.rkt")
  (require-typed/untyped "syntax-parse.rkt"
                         "sequence.rkt"
                         "typed-rackunit.rkt")
  
  (require (for-syntax syntax/parse
                       syntax/parse/experimental/template
                       racket/syntax
                       type-expander/expander
                       phc-toolkit/untyped/aliases
                       (submod "syntax-parse.rkt" untyped)
                       (submod "repeat-stx.rkt" untyped)
                       (submod "stx.rkt" untyped))
           typed/rackunit)

  
  
  (define-syntax/parse (check-ann value type:type-expand! (~optional message))
    (quasitemplate
     ((λ _ (void)) (ann value type.expanded))))
  
  (: check-equal?-classes (∀ (A ...) (→ (Pairof String (Listof A)) ... Void)))
  (define (check-equal?-classes . classes)
    (for* ([(head tail) (in-split* classes)])
      (let ([this-class (sequence-ref tail 0)]
            [different-classes (in-sequences head (sequence-tail tail 1))])
        (for ([val (cdr this-class)])
          (for ([other-val (cdr this-class)])
            #;(displayln (format "Test ~a ∈ ~a = ~a ∈ ~a …"
                                 val
                                 (car this-class)
                                 other-val
                                 (car this-class)))
            (check-equal?: val other-val
                           (format "Test ~a ∈ ~a = ~a ∈ ~a failed."
                                   val
                                   (car this-class)
                                   other-val
                                   (car this-class))))
          (for ([different-class different-classes])
            (for ([different-val (cdr different-class)])
              #;(displayln (format "Test ~a ∈ ~a != ~a ∈ ~a ∈ ~a …"
                                   val
                                   (car this-class)
                                   different-val
                                   (car different-class)
                                   (map (λ ([c : (Pairof String Any)])
                                          (car c))
                                        (sequence->list
                                         different-classes))))
              (check-not-equal?: val different-val
                                 (format "Test ~a ∈ ~a != ~a ∈ ~a ∈ ~a failed."
                                         val
                                         (car this-class)
                                         different-val
                                         (car different-class)
                                         (map (λ ([c : (Pairof String Any)])
                                                (car c))
                                              (sequence->list
                                               different-classes))))))))))
  
  (define-syntax/parse
      (check-equal?-classes:
       [{~maybe #:name {~or name:str name-id:id}}
        ;; TODO: should be {~lit :), but still accept the ":"
        ;; from type-expander
        {~maybe :colon c-type:type-expand!}
        {~and {~or {~seq single-val-id:id {~maybe {~lit :} _}}
                   {~seq _ …}}
              {~seq {~seq val {~maybe :colon v-type:type-expand!}} …}}]
       …)
    (define/with-syntax ([a-val …] …)
      (template ([(?? (ann val v-type.expanded) val) …] …)))
    (define/with-syntax ([aa-val …] …)
      (let ()
        ;; TODO: this is ugly, repeat-stx should handle missing stuff instead.
        (define/with-syntax (xx-c-type …)
          (template ((?? (c-type.expanded) ()) …)))
        (syntax-parse (repeat-stx (xx-c-type …) ([val …] …))
          [([({~optional c-type-rep}) …] …)
           (template ([(?? (ann a-val c-type-rep) a-val) …] …))])))
    (template
     (check-equal?-classes (list (?? (?? name (symbol->string 'name-id))
                                     (?? (symbol->string 'single-val-id) ""))
                                 aa-val …) …)))

  
  ;; check-tc and check-not-tc
  (begin
    ;; Adapted from https://github.com/racket/typed-racket/issues/87
    (define-for-syntax (eval-tc checker expr [loc-stx #f])
      (quasisyntax/top-loc (or loc-stx #'here)
        (begin
          (: ns-anchor Namespace-Anchor)
          (define-namespace-anchor ns-anchor)
          #,(checker (quasisyntax/top-loc loc-stx
                       (λ ()
                         (define output (open-output-string))
                         (parameterize ([current-output-port output])
                           (eval `(#%top-interaction . #,expr)
                                 (namespace-anchor->namespace ns-anchor)))
                         (get-output-string output)))))))

    (define-syntax (check-tc stx)
      (eval-tc (λ (f) (quasisyntax/top-loc stx
                        (check-not-exn #,f)))
               (syntax-case stx ()
                 [(_ body0) #'body0]
                 [(_ . body) (syntax/top-loc stx
                               (begin . body))])
               stx))

    (define-for-syntax tc-error-regexp
      (pregexp
       (string-append
        "Type Checker: ("
        "type mismatch"
        "|Polymorphic function .*could not be applied to arguments)")))
    (define-syntax check-not-tc
      (syntax-parser
        [(_ (~optional (~seq #:message-regexp message-regexp)
                       #:defaults ([message-regexp #`#,tc-error-regexp]))
            . (~or (body₀) body*))
         (eval-tc (λ (code) (quasisyntax/top-loc this-syntax
                              (check-exn:
                               (λ (ex)
                                 (and (exn:fail:syntax? ex)
                                      (regexp-match? message-regexp
                                                     (exn-message ex))))
                               #,code)))
                  (if (attribute body₀)
                      #'body₀
                      (syntax/top-loc this-syntax
                        (begin . body*)))
                  this-syntax)]))))