;; SPDX-License-Identifier: MIT
;; Copyright (C) 2023 Massimo Zaniboni <mzan@dokmelody.org>

(defpackage :dok/compiler
  (:import-from :trivial-types
     :proper-list
     :tuple)
  (:import-from :alexandria
     :hash-table-keys)
  (:import-from :serapeum :let1)
  (:use :cl :defstar :dok/ast)
  (:export #:compile-and-load-dok-file
           #:compile-dok-file))

(in-package :dok/compiler)

;;; Compile Dok to Common Lisp code.

(defgeneric* (clos-name -> string) ((dcl I-Declaration))
  "The name of the CLOS class.")

(defmethod clos-name ((dcl I-Declaration))
  (fully-qualified-name dcl))

(defun* (compile-to-lisp -> Node) ((in stream) pkg-name (out stream))
  "Compile Dok source code to Common Lisp code.
   It emits readable Common Lisp code, so it is more simple and inspectable.
   This approach is good enough for boostrapping Dok."
  (labels ((emit (lisp-code) (print lisp-code out))

    (emit-str (str) (write-string str out))

    (ns (node)
      (intern (clos-name node)))

    (nns (node)
      (let ((nnode (parent-decl node)))
        (if nnode (list (ns nnode)) nil)))

    (walk-slots (parts)
       (loop for p in parts
             if (typep p 'Data-part/Slot-decl)
                collect (let ((sname (intern (name p))))
                          `(,sname :accessor ,sname :initform nil))
                into r
             finally (return r)))

    (walk-classes (node)
      (typecase node
        (Code (loop for p in (stmt* node)
                    do (walk-classes p)))
        (Stmt/Data-decl
          ; TODO manage type params
         (emit `(defclass ,(ns node) ,(nns node) (,@(walk-slots (data-part* node)))))
          (loop for p in (data-part* node)
                do (walk-classes p)))
        (Data-part/Variant-decl
         (emit `(defclass ,(ns node) ,(nns node) (,@(walk-slots (data-part* node)))))
          (loop for p in (data-part* node)
                do (walk-classes p)))
        (Data-part/Fun-decl
          (loop for p in (stmt* node)
                do (walk-classes p)))
        (Data-part/Nested-data-decl
           (walk-classes (stmt/data-decl node)))
        (Expr/Stmts
         (loop for p in (stmt* node)
               do (walk-classes p)))))

    (collect-vars (parts)
       (loop for p in parts
             if (typep p 'Stmt/Var-decl)
                collect `(,(intern (name p))
                          (make-instance (quote ,(intern (fully-qualified-name (type-decl (type-ref p)))))))
                into r
             finally (return r)))

    (emit-block (parts)
      (let ((vars (collect-vars parts)))

        (emit-str (format nil "~%(block nil "))

        (when vars
          (emit-str "(let ")
          (emit vars))

        (loop for p in parts
            do (walk-stmts p))

        (when vars (emit-str ")"))
        (emit-str ")")))

    (convert-expr (node)
      (etypecase node
        (Null nil)
        (Expr/Value/Integer
         (value node))
        (Expr/Value/Float
         (value node))
        (Expr/Value/String
         (value node))
        (Expr/Value/Id
         (intern (id node)))
        (Expr/Fun-call
          (loop for prev-expr = (convert-expr (expr node)) then p-expr
                for p in (fun-call-chain* node)
                for args = (loop for a in (argument* p)
                                 collect (convert-expr a))
                for p-expr = (list* (intern (id p)) prev-expr args)
                finally (return (if p-expr p-expr prev-expr))))
        (Expr/Binary-expr
         (let ((operator (operator node))
               (arg1 (convert-expr (arg1 node)))
               (arg2 (convert-expr (arg2 node))))
           (cond
             ((string-equal operator "==") `(equalp ,arg1 ,arg2))
             ((string-equal operator "+") `(+ ,arg1 ,arg2)))))))

    (walk-stmts (node)
      (etypecase node
        (Code
           (emit-block (stmt* node)))
        (Stmt/Return
           (emit `(return ,(convert-expr (expr node)))))
        (Stmt/Assert
         (emit `(assert ,(convert-expr (expr node)))))
        (Stmt/Var-decl
         (when (expr node)
           (emit `(setq ,(name node) ,(convert-expr (expr node))))))
        (Stmt/Data-Decl)))

    (walk-funs (node)
      (typecase node
        (Code (loop for p in (stmt* node)
                    do (walk-funs p)))
        (Stmt/Data-decl
          (loop for p in (data-part* node)
                do (walk-funs p)))
        (Data-part/Variant-decl
          (loop for p in (data-part* node)
                do (walk-funs p)))
        (Data-part/Nested-data-decl
           (walk-funs (stmt/data-decl node)))
        (Data-part/Fun-decl
          (emit-str (format nil "~%(defmethod |~a| ((self |~a|) " (name node) (fully-qualified-name (parent-decl node))))

          (loop for p in (fun-param-decl* node)
                do (emit `(,(intern (name p)) ,(intern (fully-qualified-name (type-decl (type-ref p)))))))

          (emit-str (format nil ")~%"))
          (emit-block (stmt* node))
          (emit-str (format nil ")~%"))
         )))

    (walk-main (node)
      (etypecase node
        (Code
         (emit-block (stmt* node)))))
    )

      (let ((ast (dok/parser:parse-dok-stream in)))
        (walk1 ast nil nil nil)
        (walk2 ast)

        (emit `(when (packagep ,pkg-name) (delete-package ,pkg-name)))
        (emit `(defpackage ,pkg-name (:use :cl :dok/runtime)))
        (emit `(in-package ,pkg-name))
        (walk-classes ast)
        (walk-funs ast)
        (walk-main ast)
        ast)))

(defun* (compile-dok-file -> (values pathname symbol Node)) ((name string))
  "Compile Dok file into Common Lisp the `src/dok/$name.dok' file. Return the produced file, the pagkage-name and the AST."

  ; TODO find the correct starting dircectory according ASDF base.
  ; Here, I'm using the "trick" that the base directory is the directory of the file,
  ; hence "src/dok/" is the final source directory.
  (let ((dok-file (uiop:merge-pathnames* (format nil "~a.dok" name) #P"dok/"))
        (cl-out (uiop:merge-pathnames* (format nil "~a.lisp" name) #P"../build/"))
        (cl-pkg (intern name "KEYWORD"))
        (ast (make-instance 'Node)))

    (with-open-file (s-in dok-file :direction :input :if-does-not-exist :error)
      (with-open-file (s-out cl-out :direction :output :if-does-not-exist :create :if-exists :supersede)
        (setq ast (compile-to-lisp s-in cl-pkg s-out))))
    (values (uiop:merge-pathnames* cl-out) cl-pkg ast)))

(defun* (compile-and-load-dok-file -> (values package pathname Node)) ((name string))
  "Compile and load into Common Lisp environment the `src/dok/$name.dok' file.
   Return the package with the loaded code and the Common Lisp file."
   (multiple-value-bind (filename cl-pkg ast) (compile-dok-file name)
     (load filename)
     (values (find-package cl-pkg) filename ast)))


(defun demo1 () (compile-and-load-dok-file "demo1"))
(defun ddemo1 () (compile-dok-file "demo1"))

