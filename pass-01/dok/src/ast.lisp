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
     #:Type-ref-path/Root
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
     #:parent-scope
     ))

(in-package :dok)

;;; The AST

(defclass Node ()
  ((parent-scope
    :accessor parent-scope
    :initarg :parent-scope
    :type (or null Node)
    :initform nil)
   (start-pos
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
    :accessor id
    :type string)
   (type-ref
    :initarg :type-ref
    :accessor type-ref
    :type (or null Type-ref))
   (cmd-rest
    :initarg :cmd-rest
    :accessor cmd-rest
    :type (or null string))
   (expr
    :initarg :expr
    :accessor expr
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

(defclass Data-part/Nested-data-decl (Data-part)
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
    :type (proper-list Type-ref-path))
   (type-decl
    :accessor type-decl
    :initform nil
    :type (or null Data-part/Variant-decl Stmt/Data-decl))))

(defclass Type-ref-path (Node) ())

(defclass Type-ref-path/Self (type-ref-path)
  ())

(defclass Type-ref-path/Root (type-ref-path)
  ()
  (:documentation "A type starting from root, i.e. `/'"))

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

(defun walk1 (node parent-scope)
  "Walk in each node of the AST, completing `parent-scope' attribute."

  (when node
    (setf (slot-value node 'parent-scope) parent-scope)
    (etypecase node
      (Code (loop for p in (stmt* node)
                  do (walk1 p node)))
      (Stmt/Return (walk1 (expr node) parent-scope))
      (Stmt/Assert (walk1 (expr node) parent-scope))
      (Stmt/Data-decl
       (loop for p in (type-param* node)
             do (walk1 p parent-scope))
       (loop for p in (data-part* node)
             do (walk1 p node)))
      (Stmt/Var-decl
       (walk1 (expr node) parent-scope)
       (walk1 (type-ref node) parent-scope))
      (Data-part/Slot-decl
       (walk1 (type-ref node) parent-scope)
       (walk1 (expr node) parent-scope))
      (Data-part/Variant-decl
       (loop for p in (data-part* node)
             do (walk1 p node)))
      (Data-part/Fun-decl
       (loop for p in (fun-param-decl* node)
             do (walk1 p parent-scope))
       (walk1 (result node) parent-scope)
       (loop for p in (stmt* node)
             do (walk1 p node)))
      (Data-part/Nested-data-decl
       (walk1 (stmt/data-decl node) node))
      (Fun-param-decl
       (walk1 (type-ref node) parent-scope))
      (Type-ref
       (loop for p in (type-ref-path* node)
             do (walk1 p parent-scope)))
      (Type-ref-path/Self nil)
      (Type-ref-path/Variant nil)
      (Type-ref-path/Data
       (loop for p in (type-param* node)
             do (walk1 p parent-scope)))
      (Type-param
       (walk1 (value node) parent-scope))
      (Expr/Value nil)
      (Expr/Default-value nil)
      (Expr/Binary-expr
       (walk1 (arg1 node) parent-scope)
       (walk1 (arg2 node) parent-scope))
      (Expr/Stmts
       (loop for p in (stmt* node)
             do (walk1 p parent-scope)))
      (Expr/Fun-call
       (walk1 (expr node) parent-scope)
       (loop for p in (fun-call-chain* node)
             do (walk1 p parent-scope)))
      (Fun-call-chain
       (loop for p in (argument* node)
             do (walk1 p parent-scope))))))

;;;
;;; Name analysis
;;;
;;; NOTE: defining a complete type-analysis is out of scope in this pass.
;;; The target is a minimal analysis for generating Common Lisp CLOS code.
;;; Many type errors will be found during Common Lisp compile time or at run-time.
;;;

(define-condition cannot-lookup-decl (error) ())

(defgeneric* (lookup-type-decl^here -> (optional Node)) ((go-nest? boolean) (node Node)  (ref Type-ref-path))
  "Given a node as scope, search the specific type-ref-path element only in the node, optionally its nested nodes (in case of `go-nest?'),
   but not in parent scopes.")

(defun* result-or-cannot-lookup-decl-error ((r (or null Node)))
  (or r (error 'cannot-lookup-decl)))

(defun* (lookup-type-decl^up -> Node) ((node Node) (ref Type-ref-path))
   "Given a node as scope, search the starting type-ref-path `A' of a type-ref like `A/B/C.D'
    in the node and one of its parent scopes."

  (result-or-cannot-lookup-decl-error
    (loop for n = node then p
        while n
        for p = (parent-scope n)
        for r = (lookup-type-decl^here t n ref)
        thereis r)))


(defun* (lookup-type-decl^down -> Node) ((node Node) (refs (proper-list Type-ref-path)))
  "Given a node `A' as scope, search the next type-ref-path `B' of a type-ref like `A/B/C.D'
   in one of its nested nodes."

  (loop for ref in refs
        for n = node then r
        for r = (lookup-type-decl^here t n ref)
        when (null r) do (error 'cannot-lookup-decl)
        finally (return r)))

(defun* (root-parent-scope -> Node) ((node Node))
  (loop for n = node then p
        for p = (parent-scope n)
        while p
        finally (return n)))

(defmethod lookup-type-decl^here (go-nest? (node Node) ref) nil)

(defmethod lookup-type-decl^here (go-nest? (node Stmt/Data-decl) (ref Type-ref-path/Variant))
  (loop for p in (data-part* node)
        thereis (lookup-type-decl^here nil p ref)))

(defmethod lookup-type-decl^here (go-nest? (node data-part/Nested-data-decl) (ref Type-ref-path/Variant))
  (lookup-type-decl^here go-nest? (stmt/data-decl node) ref))

(defmethod lookup-type-decl^here (go-nest? (node Data-part/Variant-decl) (ref Type-ref-path/Variant))
  (or (if (string-equal (name node) (name ref)) node)
      (if go-nest?
          (loop for p in (data-part* node)
                thereis (lookup-type-decl^here nil p ref)))))

(defmethod lookup-type-decl^here (go-nest? (node Stmt/Data-decl) (ref Type-ref-path/Data))
  (or (if (string-equal (name node) (name ref)) node)
      (if go-nest?
          (loop for p in (type-param* node)
                thereis (string-equal (param p) (name ref)))
          (loop for p in (data-part* node)
                thereis (lookup-type-decl^here nil p ref)))))

(defmethod lookup-type-decl^here (go-nest? (node data-part/Nested-data-decl) (ref Type-ref-path/Data))
  (lookup-type-decl^here go-nest? (stmt/data-decl node) ref))

(defmethod lookup-type-decl^here (go-nest? (node Data-part/Variant-decl) (ref Type-ref-path/Data))
  (if go-nest?
      (loop for p in (data-part* node)
            thereis (lookup-type-decl^here nil p ref))))

(defmethod lookup-type-decl^here (go-nest? (node Code) (ref Type-ref-path))
  (if go-nest?
      (loop for p in (stmt* node)
            thereis (lookup-type-decl^here nil p ref))))

(defmethod lookup-type-decl^here (go-nest? (node Expr/Stmts) (ref Type-ref-path))
  (if go-nest?
      (loop for p in (stmt* node)
            thereis (lookup-type-decl^here nil p ref))))

(defun* (lookup-type-decl -> Node) ((node Node) (ref Type-ref))
  "Given a node as scope, and a type-ref, search the node where the type-ref is declarated.
   Given a type-ref like `A/B/C.D', it is equivalent to searching first a declaration of `A'
   following parent scopes if necessary, then searhing `B', `C' and `D' following
   nested (i.e. `down') nodes."

  (let* ((refs (type-ref-path* ref))
         (ref1 (car refs))
         (root? (typep ref1 'Type-ref-path/root))
         (ref* (if root? refs (cdr refs)))
         (start-node (if root?
                        (root-parent-scope node)
                        (lookup-type-decl^up node ref1))))
      (if ref*
          (lookup-type-decl^down start-node ref*)
          start-node)))

(defun walk2 (node)
  "Apply name analysis to each type-ref, linking them to their declaration node."

  (etypecase node
      (null nil)
      (Code (loop for p in (stmt* node)
                  do (walk2 p)))
      (Stmt/Return (walk2 (expr node)))
      (Stmt/Assert (walk2 (expr node)))
      (Stmt/Data-decl
       (loop for p in (type-param* node)
             do (walk2 p))
       (loop for p in (data-part* node)
             do (walk2 p)))
      (Stmt/Var-decl
       (walk2 (expr node))
       (walk2 (type-ref node)))
      (Data-part/Slot-decl
       (walk2 (type-ref node))
       (walk2 (expr node)))
      (Data-part/Variant-decl
       (loop for p in (data-part* node)
             do (walk2 p)))
      (Data-part/Fun-decl
       (loop for p in (fun-param-decl* node)
             do (walk2 p))
       (walk2 (result node))
       (loop for p in (stmt* node)
             do (walk2 p)))
      (Data-part/Nested-data-decl
       (walk2 (stmt/data-decl node)))
      (Fun-param-decl
       (walk2 (type-ref node)))
      (Type-ref
       (setf (type-decl node) (lookup-type-decl node node)))
      (Type-ref-path/Self nil)
      (Type-ref-path/Variant nil)
      (Type-ref-path/Data
       (loop for p in (type-param* node)
             do (walk2 p)))
      (Type-param
       (walk2 (value node)))
      (Expr/Value nil)
      (Expr/Default-value nil)
      (Expr/Binary-expr
       (walk2 (arg1 node))
       (walk2 (arg2 node)))
      (Expr/Stmts
       (loop for p in (stmt* node)
             do (walk2 p)))
      (Expr/Fun-call
       (walk2 (expr node))
       (loop for p in (fun-call-chain* node)
             do (walk2 p)))
      (Fun-call-chain
       (loop for p in (argument* node)
             do (walk2 p)))))
