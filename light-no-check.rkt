#lang racket/base

(provide (except-out (all-from-out racket/base)
                     define)
         (rename-out [new-: :]
                     [new-define-type define-type]
                     [new-define define]
                     [new-require/typed require/typed]))

(require (for-syntax racket/base))

(begin-for-syntax
  (define (process-arg stx)
    (syntax-case stx (new-:)
      [id/kw (or (identifier? #'id/kw) (keyword? (syntax-e #'id/kw))) #'id/kw]
      [[_ _] stx] ;; [arg default]
      [[arg new-: _] #'arg]
      [[arg new-: _ default] #'[arg default]]))
  (define (process-curried stx)
    (syntax-case stx ()
      [id (identifier? #'id) #'id]
      [(recur arg ...)
       (with-syntax ([recur.no-types (process-curried #'recur)]
                     [(arg.no-types ...)
                      (map process-arg (syntax->list #'(arg ...)))])
         #'(recur.no-types arg.no-types ...))])))

(define-syntax (new-: stx) #'(begin))
(define-syntax (new-define-type stx) #'(begin))
(define-syntax (new-define stx)
  (syntax-case stx (new-:)
    [(_ #:∀ _ curried new-: _ e ...)
     (with-syntax ([curried.no-types (process-curried #'curried)])
       #'(define curried.no-types e ...))]
    [(_ #:∀ _ curried e ...)
     (with-syntax ([curried.no-types (process-curried #'curried)])
       #'(define curried.no-types e ...))]
    [(_ curried new-: _ e ...)
     (with-syntax ([curried.no-types (process-curried #'curried)])
       #'(define curried.no-types e ...))]
    [(_ curried e ...)
     (with-syntax ([curried.no-types (process-curried #'curried)])
       #'(define curried.no-types e ...))]))

(define-syntax-rule (new-require/typed mod [id τ] ...)
  (require (only-in mod id ...)))