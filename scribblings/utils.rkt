#lang racket

(require scribble/manual
         (for-syntax syntax/parse))

(provide def-orig)

(define-syntax def-orig
  (syntax-parser
    [(_ orig:id [lib ...] o:id ...)
     #`(begin
         (module orig racket/base
           (require scribble/manual)
           (require (for-label lib ...))
           (define o (racket o))
           ...
           (provide (prefix-out orig (prefix-out : o)) ...))
         #,(datum->syntax #'orig `(require (quote ,#'orig))))]))