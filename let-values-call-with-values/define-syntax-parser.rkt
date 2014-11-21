#lang racket/base

(provide define-syntax-parser)

(require syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     ))

;; define-syntax-parser idea from
;; https://github.com/jackfirth/generic-bind/commit/863d39229fb42395face91de733870c0d02f0922#diff-3252674930bbd0c4e113856a2a3a5747R118
(define-simple-macro (define-syntax-parser id:id option-or-clause ...)
  (define-syntax id (syntax-parser option-or-clause ...)))

