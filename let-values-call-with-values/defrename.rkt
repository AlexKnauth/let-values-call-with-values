#lang racket/base

(provide defrename)

(require "define-syntax-parser.rkt"
         (for-syntax racket/base
                     syntax/parse
                     ))

(define-syntax-parser defrename
  [(defrename id1:id id2:id)
   #'(define-syntax id1 (make-rename-transformer #'id2))]
  [(defrename [id1:id id2:id] ...)
   #'(begin (defrename id1 id2) ...)])

