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

(define (not-html-comment? x)
  (not (and (list? x)
            (not (empty? x))
            (eq? '!HTML-COMMENT (first x)))))

(define (remove-copyright expr)
  (if (list? expr)
      (map remove-copyright (filter not-html-comment? expr))
      expr))

(define (insert-pdr-file f1)
  (let* ([f2 (string->path (string-append "../docs/decisions/" f1))]
         [c1 (parse-markdown f2)]
         [c2 (remove-copyright c1)])
    (xexprs->scribble-pres c2)))

