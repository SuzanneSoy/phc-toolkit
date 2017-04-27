#lang racket/base
(provide include-without-first-line)

(require (for-syntax racket/base))

(define-for-syntax (replace-context ctx stx)
  (define (recur e)
    (cond
      [(syntax? e) (datum->syntax ctx (recur (syntax-e e)) e e)]
      [(pair? e) (cons (recur (car e)) (recur (cdr e)))]
      [(null? e) e]
      [(vector? e) ((if (immutable? e)
                        vector->immutable-vector
                        (λ (v) v))
                    (list->vector
                     (recur (vector->list e))))]
      [(hash? e) ((if (immutable? e)
                      (cond [(hash-eq? e) make-immutable-hasheq]
                            [(hash-eqv? e) make-immutable-hasheqv]
                            [else make-immutable-hash])
                      (cond [(hash-eq? e) make-hasheq]
                            [(hash-eqv? e) make-hasheqv]
                            [else make-hash]))
                  (recur (hash->list e)))]
      [(prefab-struct-key e) => (λ (k)
                                  (apply make-prefab-struct
                                         k
                                         (recur (cdr
                                                 (vector->list
                                                  (struct->vector e))))))]
      [(box? e) ((if (immutable? e) box-immutable box)
                 (recur (unbox e)))]
      [else e]))
  (recur stx))

(define-syntax (include-without-first-line stx)
  (syntax-case stx ()
    [(_ filename1-stx . filename+-stx)
     (let*-values ([(user-filename) (map syntax-e
                                         (syntax->list
                                          #'(filename1-stx . filename+-stx)))]
                   [(base _1 _2) (split-path (syntax-source #'filename1-stx))]
                   [(filename) (apply build-path base user-filename)])
       (with-input-from-file filename
         (λ ()
           (read-line) ;; discard the first line.
           (replace-context
            #'filename1-stx
            #`(begin
                . #,(for/list ([rd (in-producer read-syntax eof filename)])
                      rd))))))]))