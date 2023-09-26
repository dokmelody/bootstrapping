;; SPDX-License-Identifier: MIT
;; Copyright (C) 2023 Massimo Zaniboni <mzan@dokmelody.org>

(defpackage :dok-parser
  (:import-from :trivial-types
     :proper-list
     :tuple)
  (:import-from :alexandria
     :hash-table-keys)
  (:import-from :serapeum :let1)
  (:use :cl :dok-ast :esrap)
  (:export :parse-dok)
  )

; TODO remove not used packages

(in-package :dok-parser)

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-integer (string)
  (when (find-if-not #'digit-char-p string)
    t))

(defrule WS (or #\space #\tab)
  (:constant nil))

(defrule WS* (* WS)
  (:constant nil))

(defrule WS+ (+ WS)
  (:constant nil))

(defrule NL+
    (and WS* (or #\newline #\return) (* (or WS #\newline #\return)))
  (:constant nil))

(defrule SYMBOL-ID
    (or "-" "_" "<" ">"))

(defrule START-ID
    (lower-case-p character))

(defrule REST-ID
    (or (alphanumericp character)
        SYMBOL-ID))

(defrule ID
    (and START-ID (* REST-ID)))

(defrule START-TYPE-ID
    (upper-case-p character))

(defrule TYPE-ID
    (and START-TYPE-ID (* REST-ID)))

(defrule REF-ID
    (and "$" ID))

(defrule OPERATOR-START-SYMBOL
    (or "=" "+" "-" "/" "*" "_" "<" ">"))

(defrule OPERATOR-SYMBOL
    (or OPERATOR-START-SYMBOL
        "." ":" "#" "$" "!" "(" ")" "{" "}" "[" "]"
        (alphanumericp character)))

(defrule OPERATOR
    (and OPERATOR-START-SYMBOL (* OPERATOR-SYMBOL)))

(defrule FLOAT-VALUE
    (and INTEGER-VALUE "." INTEGER-VALUE))

(defrule INTEGER-VALUE
    (and (? "-") (+ (digit-char-p character))))

(defrule STRING-VALUE
  (and #\" (* (not-doublequote character)) #\"))

(defrule CMD-REST
    (and "-" ID))

(defrule code stmts)

(defrule stmts
    (or (and NL+ (+ (and stmt NL+)))
        (and stmt WS*)))

(defrule stmt
    (or (and "return" WS+ expr)
        (and "assert" WS+ expr)
        type-decl
        var-decl))

(defrule type-decl
    (and "data" WS+ TYPE-ID (or WS+ (and "(" WS* type-params ")" WS+)) (? CMD-REST) WS* (? (and "[" WS* data-content "]" WS*))))

(defrule data-content
    (or (and NL+ (* (and data-part NL+)))
        (and data-part (* (and "," data-part)))))

(defrule data-part
    (or slot-decl
        variant-decl
        fun-decl
        type-decl))

(defrule slot-decl
    (and "slot" WS+ ID (? (and WS* "::" WS* type-ref)) WS* (? expr)))

(defrule variant-decl
    (and "variant" WS+ "../" TYPE-ID (? (and WS+ CMD-REST WS+)) (? (and WS* "[" WS* data-content "]" WS*))))

(defrule fun-decl
    (and "fun" WS+ ID (? (and "(" WS* params-decl ")" WS*)) WS* "::" WS* type-ref WS+ (? (and CMD-REST WS+)) "{" WS* stmts "}" WS*))

(defrule params-decl
    (+ (and NL+ param-decl)))

(defrule param-decl
    (and ID WS* "::" WS* type-ref))

(defrule expr
    (or binary-expr
        unary-expr))

(defrule binary-expr
    (and unary-expr OPERATOR WS+ unary-expr))

(defrule unary-expr
    (or (and "{" WS* stmts "}" WS*)
        fun-call-chain
        (and "(" WS* expr ")" WS*)
        value))

(defrule value
    (and (or float-value
             integer-value
             string-value
             "self"
             ID) WS*))

(defrule fun-call-chain
    (and value (+ (and "." fun-call-chain-part))))

(defrule fun-call-chain-part
    (and ID (? (and "(" WS* expr (* (and "," WS* expr)) ")")) WS*))

(defrule var-decl
    (and "var" WS+ ID WS* (? (and "::" WS* type-ref)) (? (and WS+ CMD-REST WS+)) (? expr)))

(defrule type-ref
    (and type-ref-start (* type-ref-path)))

(defrule type-ref-start
    (or "Self"
        (and "../" TYPE-ID)
        (and TYPE-ID (? (and "(" WS* type-params ")")))))

(defrule type-ref-path
    (or (and "." TYPE-ID (? (and "(" WS* type-params ")" WS*)))
        (and "/" TYPE-ID)))

(defrule type-params
    (or (and type-param (* (and "," WS* type-param)))
        (and (+ (and NL+ type-param)) NL+)))

(defrule type-param
    (and TYPE-ID "::" type-ref))

; (describe-grammar 'stmts)
; (trace-rule 'expr :recursive t)
; (trace-rule 'stmt :recursive t)
; (trace-rule 'stmts :recursive t)
; (trace-rule 'WS+ :recursive t)

;;; Utility rules.

;;; Here we go: an S-expression is either a list or an atom, with possibly leading whitespace.

; (describe-grammar 'sexp)
; (trace-rule 'sexp :recursive t)
; (parse 'sexp "(foo bar 1 quux)")
; (untrace-rule 'sexp :recursive t)

;; Exported entry points

(defun parse-dok (src)
  (parse 'Code src))
