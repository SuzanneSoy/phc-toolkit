#lang typed/racket
(require "typed-untyped.rkt")
(define-typed/untyped-modules #:no-test
  (provide % define% in let1)

  (require (for-syntax syntax/parse
                       "typed-untyped.rkt")
           "in.rkt")
  (begin-for-syntax
    (if-typed (require phc-toolkit/aliases)
              (require phc-toolkit/untyped/aliases)))

  (define-syntax-rule (let1 var val . body)
    (let-values ([(var) val]) . body))
  
  #|(define-syntax (% stx)
  (syntax-parse stx #:literals (= → :)
    [(_ (~seq (~or ((~and var (~not :)) ...)
                   (~seq (~and var (~not (~or = → :))) ...)) = expr)
        ...
        (~optional (~literal →)) . body)
     #'(let-values ([(var ...) expr] ...) . body)]))|#

  (define-for-syntax mymatch
    (syntax-parser
      #:literals (cons list vector syntax)
      ;; TODO: use define/with-syntax if we are in syntax mode.
      [(_ val (v:id)) #'(define v val)]
      [(_ val (cons a b)) #'(begin (mymatch (car val) a)
                                   (mymatch (car val) b))]
      [(_ val null)
       #'(assert val null?)]
      ;; TODO: handle ellipses
      [(_ val (list pat ...))
       #'(mymatch val (list* pat ... null))]
      [(_ val (list* pat ... rest-pat))
       #:with (tmp* ...) (generate-temporaries #'(list pat ...))
       #:with (tmp ... _) #'(tmp* ...)
       #:with (_ new-tmp ...) #'(tmp* ...)
       #:with (first . _) #'(tmp* ...)
       #:with (_ ... last) #'(tmp* ...)
       #'(begin
           (define first val)
           (begin (mymatch (car tmp) pat)
                  (define new-tmp (cdr tmp)))
           ...
           (mymatch last rest-pat))]))
  
  (begin-for-syntax
    (define-syntax-class %pat
      (pattern v:id
               #:with expanded #'v)
      (pattern ()
               #:with expanded #'(list))
      (pattern ({~literal unsyntax} x:%pat)
               #:with expanded #'(app syntax-e x.expanded))
      (pattern (x:%pat . rest:%pat)
               #:with expanded #'(cons x.expanded rest.expanded))
      (pattern #(x:%pat …)
               #:with expanded #'(vector x.expanded …)))
    (define-splicing-syntax-class %assignment
      #:attributes ([pat.expanded 1] [expr 0])
      #:literals (= in)
      (pattern (~seq (~and maybe-pat (~not (~or = in))) ...
                     (~datum =) expr:expr)
               #:with [pat:%pat ...] #'(maybe-pat ...))))
  
  (define-syntax (% stx)
    (syntax-parse stx #:literals (= in)
      [(_ :%assignment ... (~optional (~literal in)) . body)
       #'(match-let*-values ([(pat.expanded ...) expr] ...)
                            . body)]))
  
  (begin-for-syntax
    (define-syntax-class typed-pat
      (pattern [x:%pat (~literal :) type:expr]
               #:with (tmp) (generate-temporaries #'(x))
               #:with var-type #`[tmp : type]
               #:with (expanded ...) #'([x.expanded tmp]))
      (pattern x:id
               #:with var-type #'x
               #:with (expanded ...) #'())
      (pattern x:%pat
               #:with (tmp) (generate-temporaries #'(x))
               #:with var-type #'tmp
               #:with (expanded ...) #'([x.expanded tmp]))))
  
  (define-syntax (define% stx)
    (syntax-parse stx
      [(_ (name param:typed-pat ...)
          (~and (~seq ret ...) (~optional (~seq (~literal :) ret-type)))
          . body)
       #'(define (name param.var-type ...)
           (match-let (param.expanded ... ...) ret ... . body))]))
  
  #|
  (begin-for-syntax
    (define-syntax-class λ%expr
      (pattern e:id #:where (symbol->string e))
      (pattern e)
      (pattern (e . rest:λ%expr))))
  
  (define-syntax (λ% stx)
    (syntax-parse stx
      [(_ expr )]))
  |#)