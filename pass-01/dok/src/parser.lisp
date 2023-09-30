;; SPDX-License-Identifier: MIT
;; Copyright (C) 2023 Massimo Zaniboni <mzan@dokmelody.org>

(defpackage :dok-parser
  (:import-from :trivial-types
     :proper-list
     :tuple)
  (:use :cl :esrap :parse-float)
  (:export :parse-dok))

; TODO recognize comments

(in-package :dok-parser)

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-integer (string)
  (when (find-if-not #'digit-char-p string)
    t))

(defun operator-startp (c)
    (member c (list #\= #\+ #\- #\/ #\* #\_ #\< #\>)))

(defun operator-middlep (c)
  (member c (list #\. #\: #\# #\$ #\! #\( #\) #\{ #\} #\[ #\])))

(defun symbol-id-start-p (c)
    (or (lower-case-p c)
        (equal c #\_)))

(defun symbol-id-p (c)
  (or (alphanumericp c)
      (member c (list #\- #\_ #\< #\>))))

(defun convert-chars (f chars)
  (funcall f (coerce (remove-if #'null chars) 'string)))

(defrule WS (or #\space #\tab)
  (:constant nil))

(defrule WS* (* WS)
  (:constant nil))

(defrule WS+ (+ WS)
  (:constant nil))

(defrule NL+
    (and WS* (or #\newline #\return) (* (or WS #\newline #\return)))
  (:constant nil))

(defrule REST-ID
    (or (alphanumericp character)
        SYMBOL-ID))

(defrule ID
    (and (symbol-id-start-p character) (* (symbol-id-p character)))
  (:destructure (c1 cs)
     (coerce (list* c1 cs) 'string)))

(defrule START-TYPE-ID
    (upper-case-p character))

(defrule TYPE-ID
    (and START-TYPE-ID (* REST-ID))
  (:destructure (c1 cs)
     (coerce (list* c1 cs) 'string)))

(defrule REF-ID
    (and "$" ID)
  (:destructure (c1 cs) cs))

(defrule OPERATOR
    (and (operator-startp character)
         (* (or (operator-startp character)
                (operator-middlep character)
                (alphanumericp character))))
    (:destructure (c1 cs)
     (coerce (list* c1 cs) 'string)))

(defrule FLOAT-VALUE
    (and INTEGER-VALUE "." INTEGER-VALUE)
  (:destructure (v1 ignore1 v2)
    (make-instance
       'dok:Expr/Value/Float
       :value (parse-float (format nil "~a.~a" (dok:value v1) (dok:value v2))))))

(defrule INTEGER-VALUE
    (and (? "-") (+ (digit-char-p character)))
    (:destructure (sign cs)
      (make-instance
         'dok:Expr/Value/Integer
         :value (let ((v (convert-chars #'parse-integer cs)))
                     (if sign (- 0 v) v)))))

(defrule STRING-VALUE
  (and #\" (* (not-doublequote character)) #\")
  (:destructure (ignore1 cs ignore2)
    (make-instance
       'dok:Expr/Value/String
       :value (coerce cs 'string))))

(defrule CMD-REST
    (and "-" ID)
    (:destructure (c1 cs)
      (format nil "-~a" cs)))

(defrule code stmts)

(defrule stmts
    (or stmts-multi-line
        stmts-single-line))

(defrule stmts-single-line
    (and stmt WS*)
    (:destructure (stmt ignore1)
      (list stmt)))

(defrule stmts-multi-line
  (and NL+ (+ (and stmt NL+)))
  (:destructure (ignore1 stmts)
    (mapcar #'first stmts)))

(defrule stmt
    (or stmt-return
        stmt-assert
        type-decl
        var-decl))

(defrule stmt-return
    (and "return" WS+ expr)
    (:destructure (ignore1 ignore2 expr)
      (make-instance 'dok:Stmt/Return :expr expr)))

(defrule stmt-assert
    (and "assert" WS+ expr)
    (:destructure (ignore1 ignore2 expr)
      (make-instance 'dok:Stmt/Assert :expr expr)))

(defrule type-decl
  (and "data" WS+ TYPE-ID (or WS+ (and "(" WS* type-params ")" WS+)) (? CMD-REST) WS* (? (and "[" WS* data-content "]" WS*)))
  (:destructure (ignore1 ignore2 type-id maybe-params maybe-cmd ignore3 maybe-content)
    (make-instance
       'dok:Stmt/Data-decl
       :name type-id
       :type-param* (third maybe-params)
       :cmd-rest maybe-cmd
       :data-part* (third maybe-content))))

(defrule data-content
    (or data-content-multi-line data-content-one-line))

(defrule data-content-one-line
  (and data-part (* (and "," WS* data-part)))
  (:destructure (p1 ps)
    (list* p1 (mapcar #'fourth ps))))

(defrule data-content-multi-line
  (and NL+ (* (and data-part NL+)))
  (:destructure (ignore1 parts)
    (mapcar #'first parts)))

(defrule data-part
    (or slot-decl
        variant-decl
        fun-decl
        type-decl))

(defrule slot-decl
  (and "slot" WS+ ID (or WS+ (and "::" type-ref)) (? expr))
  (:destructure (ignore1 ignore2 slot-id maybe-type-ref maybe-expr)
    (make-instance
     'dok:Data-part/Slot-decl
     :id slot-id
     :type-ref (second maybe-type-ref)
     :expr maybe-expr)))

(defrule variant-decl
  (and "variant" WS+ "../" TYPE-ID (? (and WS+ CMD-REST WS+)) (? (and WS* "[" WS* data-content "]" WS*)))
  (:destructure (ignore1 ignore2 ignore3 type-id maybe-cmd maybe-data)
    (make-instance
       'dok:Data-part/Variant-decl
       :name type-id
       :cmd-rest (second maybe-cmd)
       :data-part* (fourth maybe-data))))

(defrule fun-decl
    (and "fun" WS+ ID (? fun-params-decl)
         "::" type-ref WS+ (? (and CMD-REST WS+)) "{"
         WS* stmts
         "}" WS*)
   (:destructure (ignore1 ignore2 fun-id params ignore3 type-ref ignore4 maybe-cmd ignore5 ignore6 stmts ignore7 ignore8)
   (make-instance
     'dok:Data-part/Fun-decl
     :id fun-id
     :fun-param-decl* params
     :result type-ref
     :cmd-rest (second maybe-cmd)
     :stmt* stmts)))

(defrule fun-params-decl
  (and "(" WS* params-decl ")" WS*)
  (:destructure (ignore1 ignore2 params ignore3 ignore4)
    params))

(defrule params-decl
    (or params-decl-multi-line params-decl-one-line))

(defrule params-decl-one-line
  (and param-decl (* (and "," WS* param-decl)))
  (:destructure (p1 ps)
    (list* p1 (mapcar #'fourth ps))))

(defrule params-decl-multi-line
  (+ (and NL+ param-decl))
  (:destructure (params)
    (mapcar #'second params)))

(defrule param-decl
  (and ID "::" type-ref)
  (:destructure (id ignore1 type-ref)
    (make-instance 'dok:fun-param-decl :id id :type-ref type-ref)))

(defrule expr
    (or binary-expr
        unary-expr))

(defrule binary-expr
  (and unary-expr OPERATOR WS+ unary-expr)
  (:destructure (arg1 operator ignore1 arg2)
    (make-instance
       'dok:Expr/Binary-expr
       :arg1 arg1
       :operator operator
       :arg2 arg2)))

(defrule unary-expr
    (or expr-as-stmts
        fun-call-chain
        priority-expr
        value))

(defrule priority-expr
  (and "(" WS* expr ")" WS*)
  (:destructure (ignore1 ignore2 expr ignore3 ignore4)
    expr))

(defrule expr-as-stmts
  (and "{" WS* stmts "}" WS*)
  (:destructure (ignore1 ignore2 stmts ignore3 ignore4)
    (make-instance
       'dok:Expr/Stmts
       :stmt* stmts)))

(defrule value
    (and (or float-value
             integer-value
             string-value
             self-value
             id-value) WS*)
  (:destructure (v ignore1) v))

(defrule self-value "self"
  (:constant (make-instance 'dok:Expr/Value/Self)))

(defrule id-value (and ID)
  (:destructure (id-name)
    (make-instance 'dok:Expr/Value/Id :id id-name)))

(defrule fun-call-chain
    (and value (+ (and "." fun-call-chain-part)))
    (:destructure (value parts)
      (make-instance
         'dok:Expr/Fun-call
         :expr value
         :fun-call-chain* (mapcar #'second parts))))

(defrule fun-call-chain-part
    (and ID (? arguments) WS*)
  (:destructure (fun-id arguments ignore1)
    (make-instance
     'dok:Fun-call-chain
     :id fun-id
     :argument*  arguments)))

(defrule arguments
   (and "(" WS* expr (* (and "," WS* expr)) ")")
  (:destructure (ignore1 ignore2 expr1 exprs ignore3)
    (list* expr1 (mapcar #'third exprs))))

(defrule var-decl
    (and "var" WS+ ID (or WS+ (and "::" type-ref)) (? (and CMD-REST WS+)) (? expr))
  (:destructure (ignore1 ignore2 id maybe-type maybe-cmd maybe-expr)
    (make-instance
       'dok:stmt/var-decl
       :id id
       :type-ref (second maybe-type)
       :cmd-rest (first maybe-cmd)
       :expr maybe-expr)))

(defrule type-ref
  (and type-ref-start (* type-ref-path))
  (:destructure (t1 ts)
    (make-instance 'dok:Type-ref :type-ref-path* (list* t1 ts))))

(defrule type-ref-start
    (or type-ref-self type-ref-variant type-ref-data))

(defrule type-ref-self
    (and "Self")
    (:destructure (ignore1)
            (make-instance 'dok:Type-ref-path/Self)))

(defrule type-ref-variant
  (and "../" TYPE-ID)
  (:destructure (ignore1 type-id)
    (make-instance
       'dok:Type-ref-path/Variant
       :name type-id)))

(defrule type-ref-data
  (and TYPE-ID (? (and "(" WS* type-params ")")))
  (:destructure (type-id params)
     (make-instance
        'dok:Type-ref-path/Data
        :name type-id
        :type-param* (mapcar #'third params))))

(defrule type-ref-path
    (or type-ref-path-data type-ref-path-variant))

(defrule type-ref-path-data
    (and "." TYPE-ID (? (and "(" WS* type-params ")" WS*)))
  (:destructure (ignore1 ignore2 type-id params)
    (make-instance
     'dok:Type-ref-path/Data
     :name type-id
     :type-param* (mapcar #'third params))))

(defrule type-ref-path-variant
  (and "/" TYPE-ID)
  (:destructure (ignore1 type-id)
    (make-instance 'dok:Type-ref-path/Variant :name type-id)))

(defrule type-params
  (or type-params-one-line type-params-multi-line))

(defrule type-params-one-line
  (and type-param (* (and "," WS* type-param)))
  (:destructure (p1 ps)
    (list* p1 (mapcar #'fourth ps))))

(defrule type-params-multi-line
  (and (+ (and NL+ type-param)) NL+)
  (:destructure (ps ignore1)
    (mapcar #'second (first ps))))

(defrule type-param
    (and TYPE-ID "::" type-ref)
    (:destructure (type-id ignore1 type-ref)
      (make-instance 'Type-param :param type-id :value type-ref)))

; TODO
; (describe-grammar 'stmts)
; (trace-rule 'expr :recursive t)
; (trace-rule 'stmt :recursive t)
; (trace-rule 'stmts :recursive t)
; (trace-rule 'WS+ :recursive t)
; (describe-grammar 'sexp)
; (trace-rule 'sexp :recursive t)
; (parse 'sexp "(foo bar 1 quux)")
; (untrace-rule 'sexp :recursive t)

;; Exported entry points

(defun parse-dok (src)
  (parse 'Code src))
