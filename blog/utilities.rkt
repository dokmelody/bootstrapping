#lang racket

;; SPDX-License-Identifier: LGPL-3.0-or-later
;; Copyright (C) 2019 Massimo Zaniboni <mzan@dokstar.org>

(require (except-in xml xexpr->string))

(require markdown
         racket/file
         markdown/scrib
         racket/class
         racket/format
         racket/runtime-path
         scribble/base
         scribble/decode
         )

(provide insert-pdr-file)

;; not used anymore
(define (remove-empty-paragrahps expr)
  (if (list? expr)
      (if 
       (and (element? expr)
            (eq? (element-name expr) 'p)
            (pcdata? (element-content expr))
            (eq? 0 (string-length (pcdata-string (element-content expr))))) 
       (make-comment "")
       (map remove-empty-paragrahps expr))
      expr))

(define (insert-pdr-file f1)
  (let* ([f2 (string->path (string-append "../docs/decisions/" f1))])
    (xexprs->scribble-pres (parse-markdown f2))))

