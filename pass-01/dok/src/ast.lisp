;; SPDX-License-Identifier: MIT
;; Copyright (C) 2023 Massimo Zaniboni <mzan@dokmelody.org>

(defpackage :dok
  (:import-from :trivial-types
     :proper-list
     :tuple)
  (:import-from :alexandria
     :hash-table-keys)
  (:import-from :serapeum :let1)
  (:use :cl :defstar)
  (:export
     #:Code
     #:Stmt
     #:Stmt/Return
     #:Stmt/Assert
     #:Stmt/Data-decl
     #:Stmt/Var-decl
     #:Data-part
     #:Data-part/Slot-decl
     #:Data-part/Variant-decl
     #:Data-part/Fun-decl
     #:Nested-data-decl
     #:Fun-param-decl
     #:Type-ref
     #:Type-ref-path
     #:Type-ref-path/Self
     #:Type-ref-path/Variant
     #:Type-ref-path/Data
     #:Type-param
     #:Expr
     #:Expr/Default-value
     #:Expr/Value
     #:Expr/Value/Integer
     #:Expr/Value/Float
     #:Expr/Value/String
     #:Expr/Value/Self
     #:Expr/Value/Id
     #:Expr/Binary-expr
     #:Expr/Stmts
     #:Expr/Fun-call
     #:Fun-call-chain
     #:value
     #:id
     #:param
     #:start-pos #:end-pos
     #:expr
     #:stmt*
     #:name
     #:type-param*
     #:cmd-rest
     #:data-part*
     #:type-ref
     #:fun-param-decl*
     #:result
     #:type-ref-path*
     #:arg1 #:arg2 #:operator
     #:fun-call-chain*
     #:argument*
     ))

(in-package :dok)

(defclass Node ()
  ((start-pos
    :accessor start-pos
    :initarg :start-pos
    :type integer
    :initform 0)
   (end-pos
    :accessor end-pos
    :initarg :end-pos
    :type integer
    :initform 0))
  )

(defclass Code (Node)
  ((stmt*
    :accessor stmt*
    :initarg :stmt*
    :type (proper-list Stmt))))

(defclass Stmt (Node) ())

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
    :type (or null string))
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
    :type (or null Type-ref))
   (cmd-rest
    :initarg :cmd-rest
    :type (or null string))
   (expr
    :initarg :expr
    :type (or null Expr))))

(defclass Data-part (Node)
  ())

(defclass Data-part/Slot-decl (Data-part)
  ((id
    :accessor id
    :initarg :id
    :type string)
   (type-ref
    :accessor type-ref
    :initarg :type-ref
    :type (or null Type-Ref))
   (expr
    :accessor expr
    :initarg :expr
    :type (or null Expr))))

(defclass Data-part/Variant-decl (Data-part)
  ((name
    :accessor name
    :initarg :name
    :type string)
   (cmd-rest
    :accessor cmd-rest
    :initarg :cmd-rest
    :type (or null string))
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
    :type (or null string))
   (stmt*
    :accessor stmt*
    :initarg :stmt*
    :type (proper-list Stmt))))

(defclass Nested-data-decl (Data-part)
  ((stmt/data-decl
    :accessor stmt/data-decl
    :initarg :stmt/data-decl
    :type Stmt/Data-decl)))

(defclass Fun-param-decl (Node)
  ((id
    :accessor id
    :initarg :id
    :type string)
   (type-ref
    :accessor type-ref
    :initarg :type-ref
    :type (or null Type-ref))))

(defclass Type-ref (Node)
  ((type-ref-path*
    :accessor type-ref-path*
    :initarg :type-ref-path*
    :type (proper-list Type-ref-path))))

(defclass Type-ref-path (Node) ())

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

(defclass Type-param (Node)
  ((param
    :accessor param
    :initarg :param
    :type string)
   (value
    :accessor value
    :initarg :value
    :type Type-ref)))

(defclass Expr (Node) ())

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
    :type Expr)
   (fun-call-chain*
    :accessor fun-call-chain*
    :initarg :fun-call-chain*
    :type (proper-list Fun-call-chain))))

(defclass Fun-call-chain (Node)
  ((id
    :accessor id
    :initarg :id
    :type string)
   (argument*
    :accessor argument*
    :initarg :argument*
    :type (proper-list Expr))))
