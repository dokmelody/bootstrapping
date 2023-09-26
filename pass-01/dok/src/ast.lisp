;; SPDX-License-Identifier: MIT
;; Copyright (C) 2023 Massimo Zaniboni <mzan@dokmelody.org>

(defpackage :dok-ast
  (:import-from :trivial-types
     :proper-list
     :tuple)
  (:import-from :alexandria
     :hash-table-keys)
  (:import-from :serapeum :let1)
  (:use :cl :defstar)
  )

(in-package :dok-ast)

(defclass Code ()
  ((stmt*
    :accessor stmt*
    :initarg :stmt*
    :type (proper-list Stmt))))

(defclass Stmt () ())

(defclass Stmt/Return (Stmt)
  ((expr
    :accessor expr
    :initarg :expr
    :type Expr)))

(defclass Stmt/Assert (Stmt)
  ((expr
    :accessor expr
    :initarg :expr
    :type Expr)))

(defclass Stmt/Data-decl (Stmt)
  ((name
    :accessor name
    :initarg :name
    :type string)
   (type-param*
    :accessor type-param*
    :initarg :type-param*
    :type (proper-list Type-param))
   (cmd-rest
    :accessor cmd-rest
    :initarg :cmd-rest
    :type string)
   (data-part*
    :accessor data-part*
    :initarg :data-part*
    :type (proper-list Data-part))))

(defclass Stmt/Var-decl (Stmt)
  ((id
    :initarg :id
    :type string)
   (type-ref
    :initarg :type-ref
    :type Type-ref)
   (cmd-rest
    :initarg :cmd-rest
    :type string)
   (expr
    :initarg :expr
    :type Expr)))

(defclass Data-part ()
  ())

(defclass Data-part/Slot-decl (Data-part)
  ((id
    :accessor id
    :initarg :id
    :type string)
   (type-ref
    :accessor type-ref
    :initarg :type-ref
    :type Type-Ref)
   (expr
    :accessor expr
    :initarg :expr
    :type Expr)))

(defclass Data-part/Varian-decl (Data-part)
  ((name
    :accessor name
    :initarg :name
    :type string)
   (cmd-rest
    :accessor cmd-rest
    :initarg :cmd-rest
    :type string)
   (data-part*
    :accessor data-part*
    :initarg :data-part*
    :type (proper-list Data-part))))

(defclass Data-part/Fun-decl (Data-part)
  ((id
    :accessor id
    :initarg :id
    :type string)
   (fun-param-decl*
    :accessor fun-param-decl*
    :initarg :fun-param-decl*
    :type (proper-list Fun-param-decl))
   (result
    :accessor result
    :initarg :result
    :type Type-ref)
   (cmd-rest
    :accessor cmd-rest
    :initarg :cmd-rest
    :type string)
   (stmt*
    :accessor stmt*
    :initarg :stmt*
    :type (proper-list Stmt))))

(defclass Nested-data-decl (Data-part)
  ((stmt/data-decl
    :accessor stmt/data-decl
    :initarg :stmt/data-decl
    :type Stmt/Data-decl)))

(defclass Fun-param-decl ()
  ((id
    :accessor id
    :initarg :id
    :type string)
   (type-ref
    :accessor type-ref
    :initarg :type-ref
    :type Type-ref)))

(defclass Type-ref ()
  ((type-ref-path*
    :accessor type-ref-path*
    :initarg :type-ref-path*
    :type (proper-list Type-ref-path))))

(defclass Type-ref-path () ())

(defclass Type-ref-path/Self (type-ref-path)
  ())

(defclass Type-ref-path/Variant (type-ref-path)
  ((name
    :accessor name
    :initarg :name
    :type string)))

(defclass Type-ref-path/Data (Type-ref-path)
  ((name
    :accessor name
    :initarg :name
    :type string)
   (type-param*
    :accessor type-param*
    :initarg :type-param*
    :type (proper-list Type-param))))

(defclass Type-param ()
  ((param
    :accessor param
    :initarg :param
    :type string)
   (value
    :accessor value
    :initarg :value
    :type Type-ref)))

(defclass Expr () ())

(defclass Expr/Default-value (Expr) ())

(defclass Expr/Value (Expr) ())

(defclass Expr/Value/Integer (Expr/Value)
  ((value
    :accessor value
    :initarg :value
    :type integer)))

(defclass Expr/Value/Float (Expr/Value)
  ((value
    :accessor value
    :initarg :value
    :type float)))

(defclass Expr/Value/String (Expr/Value)
  ((value
    :accessor value
    :initarg :value
    :type string)))

(defclass Expr/Value/Self (Expr/Value) ())

(defclass Expr/Value/Id (Expr/Value)
  ((id
    :accessor id
    :initarg :id
    :type string)))

(defclass Expr/Binary-expr (Expr)
  ((arg1
    :accessor arg1
    :initarg :arg1
    :type Expr)
   (operator
    :accessor operator
    :initarg :operator
    :type string)
   (arg2
    :accessor arg2
    :initarg :arg2
    :type Expr)))

(defclass Expr/Stmts (Expr)
  ((stmt*
    :accessor stmt*
    :initarg :stmt*
    :type (proper-list Stmt))))

(defclass Expr/Fun-call (Expr)
  ((expr
    :accessor expr
    :initarg :expr
    :type Exrp)
   (fun-call-chain*
    :accessor fun-call-chain*
    :initarg :fun-call-chain*
    :type (proper-list Fun-call-chain))))

(defclass Fun-call-chain ()
  ((id
    :accessor id
    :initarg :id
    :type string)
   (argument*
    :accessor argument*
    :initarg :argument*
    :type (proper-list Expr))))
