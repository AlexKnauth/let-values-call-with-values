#lang racket/base

(provide let-values/call-with-values
         let*-values/call-with-values
         letrec-values/call-with-values
         define-values/call-with-values
         )

(require racket/function
         syntax/parse/define
         "define-syntax-parser.rkt"
         (for-syntax racket/base
                     syntax/parse))

(module+ test
  (require rackunit "defrename.rkt"))

(begin-for-syntax
  (define/syntax-parse ooo (quote-syntax ...))
  (define/syntax-parse ooo+ (quote-syntax ...+))
  
  (define-syntax-class fmls
    #:attributes (ids)
    [pattern rst:id                    #:with ids #'(rst)]
    [pattern (arg:arg ...)             #:with ids #'(arg.id ...)]
    [pattern (arg:arg ... . rst:id)    #:with ids #'(arg.id ... rst)]
    [pattern (arg:arg ...+ . rst:fmls) #:with ids #'(arg.id ... . rst.ids)])
  (define-splicing-syntax-class arg
    #:attributes (id)
    [pattern (~seq id:id)]
    [pattern (~seq [id:id df:expr])])
  
  (define-syntax-class lv/cwvs-binding
    #:attributes (norm)
    [pattern (~and stx [(id:id ...) expr:expr])
             #:with norm #'stx]
    [pattern (~and stx [(~and ids (id:id ...)) expr:expr ...+])
             #:with new-expr (syntax/loc #'stx
                               (let () expr ...))
             #:with norm (syntax/loc #'stx
                           [ids new-expr])]
    [pattern (~and stx [fmls:fmls expr:expr ...+])
             #:with generator (syntax/loc #'stx
                                (thunk expr ...))
             #:with receiver (syntax/loc #'stx
                               (lambda fmls
                                 (values . fmls.ids)))
             #:with norm (syntax/loc #'stx
                           [fmls.ids (call-with-values generator receiver)])]
    ))

(define-syntax-parser def-lv/cwvs
  [(def-lv/cwvs lv/cwvs:id lv:id)
   #'(define-simple-macro (lv/cwvs (binding:lv/cwvs-binding ooo) body:expr ooo+)
       (lv (binding.norm ooo) body ooo))]
  [(def-lv/cvws [lv/cwvs:id lv:id] ...)
   #'(begin (def-lv/cvws lv/cwvs lv) ...)])

(define-syntax-parser def-dv/cwvs
  [(def-dv/cwvs dv/cwvs:id dv:id)
   #'(define-simple-macro (dv/cwvs . binding:lv/cwvs-binding)
       (dv . binding.norm))]
  [(def-dv/cvws [dv/cwvs:id dv:id] ...)
   #'(begin (def-dv/cvws dv/cwvs dv) ...)])

(def-lv/cwvs
  [let-values/call-with-values let-values]
  [let*-values/call-with-values let*-values]
  [letrec-values/call-with-values letrec-values])
(def-dv/cwvs
  [define-values/call-with-values define-values])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (defrename
    [lv let-values/call-with-values]
    [l*v let*-values/call-with-values]
    [lrv letrec-values/call-with-values]
    [dv define-values/call-with-values]
    [vs values])
  (check-equal? (lv ([(a) 1]) a) 1)
  (check-equal? (lv ([(a b) (vs 1 2)]) (list a b)) (list 1 2))
  (check-equal? (lv ([(a . rst) (vs 1 2)]) (list a rst)) (list 1 (list 2)))
  (check-equal? (lv ([lst (vs 1 2)])       lst)          (list 1 2))
  (check-equal? (lv ([(a [b 3]) (vs 1 2)]) (list a b)) (list 1 2))
  (check-equal? (lv ([(a [b 3]) (vs 1)])   (list a b)) (list 1 3))
  (check-equal? (lv ([(a [b 5] . c) (vs 1 2 3 4)]) (list a b c)) (list 1 2 (list 3 4)))
  (check-equal? (lv ([(a [b 5] . c) (vs 1 2)])     (list a b c)) (list 1 2 (list)))
  (check-equal? (lv ([(a [b 5] . c) (vs 1)])       (list a b c)) (list 1 5 (list)))
  (dv (a1) 1)           (check-equal? a1 1)
  (dv (a2 b2) (vs 1 2)) (check-equal? (list a2 b2) (list 1 2))
  (dv (a3 . rst3) (vs 1 2)) (check-equal? (list a3 rst3) (list 1 (list 2)))
  (dv lst4 (vs 1 2))        (check-equal? lst4 (list 1 2))
  (dv (a5 [b5 3]) (vs 1 2)) (check-equal? (list a5 b5) '(1 2))
  (dv (a6 [b6 3]) (vs 1))   (check-equal? (list a6 b6) '(1 3))
  (dv (a7 [b7 5] . c7) (vs 1 2 3 4)) (check-equal? (list a7 b7 c7) '(1 2 (3 4)))
  (dv (a8 [b8 5] . c8) (vs 1 2))     (check-equal? (list a8 b8 c8) '(1 2 ()))
  (dv (a9 [b9 5] . c9) (vs 1))       (check-equal? (list a9 b9 c9) '(1 5 ()))
  )
