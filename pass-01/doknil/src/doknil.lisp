;; SPDX-License-Identifier: MIT
;; Copyright (C) 2022 Massimo Zaniboni <mzan@dokmelody.org>

(defpackage :dokmelody
  (:import-from :trivial-types
     :proper-list
     :tuple)
  (:import-from :alexandria
     :hash-table-keys)
  (:import-from :serapeum :let1)
  (:use :cl :defstar :trivia :parse-float :iterate :let-plus)
  (:use :postmodern :cl-postgres)
  (:export :cntx :facts? :fact!
           :def-role :def-ref-type :def-data-format :def-chunk
           :db-init))

(in-package :dokmelody)


; TODO API design conventions
; - the body of def-... are not variables with symbols
; - if one want to create them programmatically, then he will create a quasiquotation and an eval
; - the keyword params can be normal variables
; - every definition is mapped to a variable

; TODO support partial role lookup as project/task
; TODO show quickly in text mode the result of some query
; TODO for each fact return:
; - where it was initially defined;
; - the previous branch assigning a different value
; - the last branch assigning a different value


; TODO remove or set in asd only during development phases
(declaim (optimize (speed 0) (space 0) (debug 3)))

(defun db-init ()
  "Initialize the DokMelody schema."

  ;;
  ;; Prepare schema
  ;;

  ; NOTE: A DB in PG has a "public" SCHEMA and other user-definable SCHEMA.
  ; A SCHEMA contains related tables.
  (execute "DROP SCHEMA IF EXISTS dokmelody CASCADE")
  (execute "CREATE SCHEMA dokmelody")

  ; NOTE: search unqualified tables first inside dokmelody SCHEMA, then in public SCHEMA,
  ; that is the default SCHEMA of each DB.
  (execute "SET search_path to dokmelody,public")

  (execute "CREATE EXTENSION LTREE")
  (execute "CREATE EXTENSION HSTORE")

  ;;
  ;; Create tables
  ;;

  ; TODO add rule about transitive closure of facts with part-of role
  ; TODO support transitive closure of roles during queries, using the ltree semantic
  ; TODO support transitive closure of part-of using recursive VIEWS
  ; TODO support context + part-of later
  ; TODO add proper INDEX after it is clear what is needed by queries, query plans, and slow queries
  ; TODO in case of different facts in different transactions or with different cntx, create different fact-ids
  ; TODO support meta-info creating obj that are references to facts or other meta-info
  ; TODO support transactions using an ltree of open branch and set to NULL when the transaction is committed to the root level

  (execute "CREATE TABLE data_format (
     type LTREE PRIMARY KEY,
     doknil_type TEXT NOT NULL
     )")

  (execute "CREATE TABLE ref_type (
     type LTREE PRIMARY KEY,
     doknil_type TEXT NOT NULL
     )")

  (execute "CREATE TABLE chunk (
     id BIGSERIAL PRIMARY KEY,
     name TEXT NOT NULL UNIQUE,
     ref_type LTREE DEFAULT NULL,
     ref TEXT DEFAULT NULL,
     data_format LTREE DEFAULT NULL
     )")

  (execute "CREATE TABLE proposition (
     id SMALLSERIAL PRIMARY KEY,
     name TEXT UNIQUE
     )
     ")

  (execute "CREATE TABLE role (
     id SERIAL PRIMARY KEY,
     role LTREE NOT NULL UNIQUE,
     doknil_role TEXT NOT NULL,
     is_part_of BOOLEAN NOT NULL DEFAULT FALSE
     -- ^ true also if it is the child of a is_part_of role
  )
  ")

  (execute "CREATE TABLE fact (
     id BIGSERIAL PRIMARY KEY,
     subj_id BIGINT NOT NULL,
     role_id INTEGER NOT NULL,

     FOREIGN KEY (subj_id)
     REFERENCES chunk(id)
     ON DELETE RESTRICT,

     FOREIGN KEY (role_id)
     REFERENCES role(id)
     ON DELETE RESTRICT
    )")

  (execute "CREATE TABLE cntx (
     fact_id BIGINT NOT NULL,
     pos SMALLINT NOT NULL,
     -- pos 0 is the obj of a fact
     obj_id BIGINT NOT NULL,
     role_id INTEGER DEFAULT NULL,
     proposition_id SMALLINT NOT NULL,

     PRIMARY KEY (fact_id, pos),

     FOREIGN KEY (fact_id)
     REFERENCES fact(id)
     ON DELETE RESTRICT,

     FOREIGN KEY (obj_id)
     REFERENCES chunk(id)
     ON DELETE RESTRICT,

     FOREIGN KEY (role_id)
     REFERENCES role(id)
     ON DELETE RESTRICT,

     FOREIGN KEY (proposition_id)
     REFERENCES proposition(id)
     ON DELETE RESTRICT
    )")

  ; TODO add triggers for updating this when new facts are added
  ; TODO do not manage deletion of facts up to date
  (execute "CREATE TABLE obj_parents (
     obj_id BIGSERIAL NOT NULL,
     parents LTREE NOT NULL,
     --^ the parents and current id
     role_id INTEGER NOT NULL,
     --^ the root role, used for the part-of relationship

     PRIMARY KEY (obj_id, parents),

     FOREIGN KEY(obj_id)
     REFERENCES chunk(id)
     ON DELETE RESTRICT,

     FOREIGN KEY(role_id)
     REFERENCES role(id)
     ON DELETE RESTRICT
     )")

  (execute "CREATE VIEW fact_with_obj AS
    SELECT  fact.id AS fact_id,
            fact.subj_id AS subj_id,
            fact.role_id AS role_id,
            (cntx.pos IS NOT NULL) AS there_is_obj,
            cntx.obj_id AS obj_id,
            cntx.role_id AS obj_role_id,
            cntx.proposition_id AS obj_proposition
     FROM fact LEFT JOIN cntx ON cntx.fact_id = fact.id
     WHERE (cntx.pos IS NULL OR cntx.pos = 0)
")

  ; NOTE: if you change the order of SELECT fields, update also Lisp queries using this view
  (execute "CREATE VIEW fact_with_cntxs AS
     SELECT fact.id AS fact_id,
            fact.subj_id AS subj_id,
            fact.role_id AS subj_role_id,
            role.role AS subj_role,
            role.doknil_role AS subj_doknil_role,
            role.is_part_of AS subj_is_part_of,
            subj_chunk.name AS subj_chunk_name,
            subj_chunk.ref AS subj_chunk_ref,
            subj_chunk.ref_type AS subj_chunk_ref_type,
            ref_type.doknil_type AS subj_chunk_doknil_ref_type,
            subj_chunk.data_format AS subj_chunk_data_format,
            data_format.doknil_type AS subj_chunk_doknil_data_format,
            MAX(cntx.pos) AS cntx_max_pos,
            array_agg(cntx.pos ORDER BY cntx.pos) AS cntxs_pos,
            array_agg(cntx.obj_id ORDER BY cntx.pos) AS cntxs_obj_id,
            array_agg(cntx.role_id ORDER BY cntx.pos) AS cntxs_role_id,
            array_agg(cntx_role.role ORDER BY cntx.pos) AS cntxs_role,
            array_agg(cntx_role.doknil_role ORDER BY cntx.pos) AS cntxs_doknil_role,
            array_agg(cntx.proposition_id ORDER BY cntx.pos) AS cntxs_proposition_id,
            array_agg(proposition.name ORDER BY cntx.pos) AS cntxs_proposition_name,
            array_agg(cntx_chunk.name ORDER BY cntx.pos) AS cntxs_chunk_name,
            array_agg(cntx_chunk.ref_type ORDER BY cntx.pos) AS cntxs_chunk_ref_type,
            array_agg(cntx_ref_type.doknil_type ORDER BY cntx.pos) AS cntxs_chunk_doknil_ref_type,
            array_agg(cntx_chunk.ref ORDER BY cntx.pos) AS cntxs_chunk_ref,
            array_agg(cntx_chunk.data_format ORDER BY cntx.pos) AS cntxs_chunk_data_format
     FROM fact
     INNER JOIN role ON fact.role_id = role.id
     INNER JOIN chunk AS subj_chunk ON fact.subj_id = subj_chunk.id
     LEFT JOIN ref_type ON subj_chunk.ref_type = ref_type.type
     LEFT JOIN data_format ON subj_chunk.data_format = data_format.type
     LEFT JOIN cntx ON cntx.fact_id = fact.id
     LEFT JOIN role AS cntx_role ON cntx_role.id = cntx.role_id
     LEFT JOIN chunk AS cntx_chunk ON cntx.obj_id = cntx_chunk.id
     LEFT JOIN ref_type AS cntx_ref_type ON cntx_chunk.ref_type = cntx_ref_type.type
     LEFT JOIN data_format AS cntx_data_format ON cntx_data_format.type = cntx_chunk.data_format
     LEFT JOIN proposition ON cntx.proposition_id = proposition.id
     GROUP BY fact.id, subj_role, subj_doknil_role, subj_is_part_of,
              subj_chunk_name, subj_chunk_ref_type, subj_chunk_doknil_ref_type, subj_chunk_ref,
              subj_chunk_data_format, data_format.doknil_type
")
  )

; TODO delete later
(defmacro mydefun (fn fc)
  "Define a function changing its name to my-fn."
  (let ((my-fn (read-from-string (format nil "my-~a" fn))))
  `(defun ,my-fn () ,fc)
  ))

;; ;;;;;;;;;;;;;;;:;;;;;;;;;;;;;;;;;;
;; Map Doknil to Postgresql state

(defparameter *ui-query-limit* 1024
  "The maximum number of facts returned from UI queries")

(defprepared
    db-insert-data-format
    "INSERT INTO data_format(type, doknil_type)
     VALUES($1, $2)
     ON CONFLICT DO NOTHING")

(defprepared
    db-insert-ref-type
    "INSERT INTO ref_type(type, doknil_type)
     VALUES($1, $2)
     ON CONFLICT DO NOTHING")

(defprepared
    priv-db-get-or-insert-proposition
    "INSERT INTO proposition(name)
     VALUES($1)
     ON CONFLICT (name) DO UPDATE SET name = excluded.name
     RETURNING id;")

(defun db-get-proposition-id (name)
  (first (first (priv-db-get-or-insert-proposition name))))

(defprepared
    db-insert-role
    "INSERT INTO role(role, doknil_role, is_part_of)
     VALUES($1, $2, $3)
     ON CONFLICT (role)
     DO UPDATE
     SET is_part_of = excluded.is_part_of,
         doknil_role = excluded.doknil_role
     RETURNING id;")

(defprepared
    db-insert-chunk
    "INSERT INTO chunk(name, ref_type, ref, data_format)
     VALUES($1, $2, $3, $4)
     ON CONFLICT (name)
     DO UPDATE
     SET ref_type = excluded.ref_type,
         ref = excluded.ref,
         data_format = excluded.data_format
    RETURNING id;")

(defprepared
    priv-db-insert-fact
    "INSERT INTO fact(subj_id, role_id)
     VALUES($1, $2)
     RETURNING id;")

(defun db-insert-fact (subj-id role-id)
  (let* ((ids (priv-db-insert-fact subj-id role-id))
         (id (first (first ids))))

    (assert (null (cdr ids)))
    id))

(defprepared
    db-insert-cntx
    "INSERT INTO cntx(fact_id, pos, role_id, obj_id, proposition_id)
     VALUES($1, $2, $3, $4, $5)
     ON CONFLICT (fact_id, pos)
     DO UPDATE
     SET obj_id = excluded.obj_id,
         role_id = excluded.role_id,
         proposition_id = excluded.proposition_id")

(defprepared
    db-get-role-ids
    "SELECT id FROM role
     WHERE role ~ $1")

(defprepared
    db-get-chunk-ids
    "SELECT id FROM chunk
     WHERE name = $1")

(defun db-get-chunk-id (s)
  (let*  ((sid (if (stringp s) s (format nil "~a" s)))
          (ids (db-get-chunk-ids sid))
          (id (first (first ids))))
    (assert (and (not (null id)) (null (cdr ids))))
    id))

(defun* (doknil-absolute-ref? -> boolean) ((s string))
  (equal #\/ (aref s 0)))

(defun* (__doknil-decl->ltree -> string) ((s string))
  "Convert a Doknil hierarchical declaration to a valid PG ltree.

   Require absolute (i.e. starting with \"\\\") type declarations."

  (assert (doknil-absolute-ref? s))

  (subseq
   (substitute #\. #\/
               (substitute #\_ #\- s))
   1))

; TODO in case other LTREE are used, then a different conversion is needed
(defun* (__doknil-ref->abs-ltree -> string) ((s string))
  "Convert an full specified Doknil hierarchical reference to an full PG ltree path.

   Convert a relative Doknil hierarchical reference to a PG ltree search path."
  (let ((s2 (substitute #\. #\/ (substitute #\_ #\- s))))
    (if (doknil-absolute-ref? s)
        (subseq s2 1)
        (concatenate 'string "*." s2))))

(defun __doknil-ref->db-id (fun-get s)
  "Convert a relative Doknil hierarchical reference to an id to the corresponding PG entity.
   Requires that the name was already declarated and stored.
   It can be relative (i.e. not starting with \"\\\").
   It managen NULL values, returning :NULL.
   It checks that the result exists and it is unique."
   (cond
       ((equal s :NULL) :NULL)
       ((null s) :NULL)
       (t (let* ((s2 (ctypecase s
                        (string s)
                        (symbol (format nil "~a" s))))
                 (s3 (__doknil-ref->abs-ltree s2))
                 (ids (funcall fun-get s3))
                 (id (first (first ids))))
           (assert (not (null id)))
           (assert (null (cdr ids)))
           id))))

(defun* (db-get-role-id -> (or db-null integer)) ((s (or db-null null symbol string)))
  (__doknil-ref->db-id #'db-get-role-ids s))

(defprepared
    db-get-ref-types
    "SELECT type FROM ref_type
     WHERE type ~ $1")

(defun* (db-get-ref-type -> (or db-null string)) ((s (or db-null null symbol string)))
  (__doknil-ref->db-id #'db-get-ref-types s))

(defprepared
    db-get-data-formats
    "SELECT type FROM data_format
     WHERE type ~ $1")

(defun* (db-get-data-format -> (or db-null string)) ((s (or db-null null symbol string)))
  (__doknil-ref->db-id #'db-get-data-formats s))

; TODO manage also "/some/other/part/x/y"
(defun doknil-decl-walk-helper (fun-insert manage-is-part? decls)
  "Walk into the nested symbols and put into rr a list of commands for creating them into the DB."

  (let ((rr (make-hash-table)))
  (labels
      ((walk (scope is-part? xs)
             (iter (for x in xs)
                   (for x-string = (when (atom x) (format nil "~a" x)))
                   (for next-scope = (if (atom x) (concatenate 'string scope x-string) scope))
                   (for prev-scope previous next-scope initially scope)
                   (for db-x = (if (atom x) next-scope prev-scope))
                   (for db-x-ltree = (__doknil-decl->ltree db-x))
                   (for db-x-symbol = (read-from-string db-x))
                   (for is-part2? = (and manage-is-part?
                                         (or is-part?
                                             (string-equal x-string "/PART"))))
                   (after-each
                     (cond
                       ((atom x)
                        (setf (gethash
                          (if manage-is-part?
                             `(,fun-insert ,db-x-ltree ,db-x ,is-part2?)
                             `(,fun-insert ,db-x-ltree ,db-x))
                          rr) nil)

                        (setf (gethash
                          `(defparameter ,db-x-symbol (quote ,db-x-symbol))
                          rr) nil))

                       ((listp x)
                           (walk prev-scope is-part? x)))))))

    (walk "" nil decls)
    (alexandria:hash-table-keys rr))))

(defmacro def-data-format (&rest types)
  `(progn ,@(doknil-decl-walk-helper 'db-insert-data-format nil types)))

(defmacro def-ref-type (&rest types)
  `(progn ,@(doknil-decl-walk-helper 'db-insert-ref-type nil types)))

(defmacro def-role (&rest types)
  `(progn ,@(doknil-decl-walk-helper 'db-insert-role t types)))

(defmacro def-chunk (name &key ref (ref-type :NULL) (data-format :NULL))
  `(progn
     (defparameter ,name (quote ,name))

     (db-insert-chunk
       ,(format nil "~a" name)
       (db-get-ref-type ,ref-type)
       ,ref
       (db-get-data-format ,data-format))))

(defvar *doknil-cntx* '()
  "The current cntx")

; TODO support syntax where cntx role is explicit like :in book/some-book
(defun fact! (subj isa role &rest cntx)
  (assert (equal isa :isa))
  (let* ((all-cntx (append cntx *doknil-cntx*))
         (subj-str (format nil "~a" subj))
         (role-str (format nil "~a" role))
         (var-fact-id (db-insert-fact
                          (db-get-chunk-id subj-str)
                          (db-get-role-id role-str))))

      ; insert cntx and nested cntx
      (iter (for pos upfrom 0)
            (for (proposition obj) on all-cntx by #'cddr)
            (after-each
              (db-insert-cntx
                var-fact-id
                pos
                :NULL
                (db-get-chunk-id (format nil "~a" obj))
                (db-get-proposition-id (format nil "~a" proposition)))
              ))))


; TODO the last added cntx in case complete the of part and the rest the additional info (the more external)
; TODO say this in the doknil documentation
; TODO support some-role/obj
(defmacro cntx (proposition obj &body body)
  `(let ((*doknil-cntx*
           (append *doknil-cntx* (list ,proposition ,obj))))
     (progn ,@body)))

;; ;;;;;;;;;;;;;;;;;;
;; Queries

(defstruct cntx
  (pos nil :read-only t)
  (role-id nil :read-only t)
  (role nil :read-only t)
  (doknil-role nil :read-only t)
  (obj-id nil :read-only t)
  (obj-name nil :read-only t)
  (obj-ref-type nil :read-only t)
  (obj-doknil-ref-type nil :read-only t)
  (obj-ref nil :read-only t)
  (obj-data-format nil :read-only t)
  (obj-proposition nil :read-only t)
  )

(defstruct fact
  (id nil :read-only t)
  (subj-id nil :read-only t)
  (subj-name nil :read-only t)
  (subj-ref-type nil :read-only t)
  (subj-doknil-ref-type nil :read-only t)
  (subj-ref nil :read-only t)
  (subj-data-format nil :read-only t)
  (subj-doknil-data-format nil :read-only t)
  (role-id nil :read-only t)
  (role nil :read-only t)
  (doknil-role nil :read-only t)
  (is-part-of nil :read-only t)

  (cntxs nil :read-only t)
)

(defprepared
    db-get-facts-about-chunk-id
    "SELECT cntxs.*,
            (fact_with_obj.subj_id = $1) AS is_fact_on_subj
     FROM fact_with_obj
     INNER JOIN fact_with_cntxs AS cntxs ON fact_with_obj.fact_id = cntxs.fact_id
     WHERE (fact_with_obj.subj_id = $1 OR fact_with_obj.obj_id = $1)
     LIMIT $2")

(defprepared
    db-get-facts-about-chunk-id-and-role-id
    "SELECT cntxs.*,
            (fact_with_obj.subj_id = $1) AS is_fact_on_subj
     FROM fact_with_obj
     INNER JOIN fact_with_cntxs AS cntxs ON fact_with_obj.fact_id = cntxs.fact_id,
     role
     WHERE role.id = $2
     AND   fact_with_obj.subj_id = $1
     AND   subj_role <@ role.role
     LIMIT $3")

(defprepared
    db-get-facts-about-chunk-id-and-obj-id
    "SELECT cntxs.*,
            (f.subj_id = $1) AS is_fact_on_subj
     FROM fact_with_obj AS f
     INNER JOIN fact_with_cntxs AS cntxs ON f.fact_id = cntxs.fact_id,
     WHERE f.subj_id = $1 AND f.obj_id = $2
     LIMIT $3")

; TODO create queries filtering about a certain context with specific parameters

(defun db-fact-with-cntx->fact (row)
  (destructuring-bind
    (fact_id
     subj_id
     subj_role_id
     subj_role
     subj_doknil_role
     subj_is_part_of
     subj_chunk_name
     subj_chunk_ref
     subj_chunk_ref_type
     subj_chunk_doknil_ref_type
     subj_chunk_data_format
     subj_chunk_doknil_data_format
     cntx_max_pos
     cntxs_pos
     cntxs_obj_id
     cntxs_role_id
     cntxs_role
     cntxs_doknil_role
     cntxs_proposition_id
     cntxs_proposition_name
     cntxs_chunk_name
     cntxs_chunk_ref_type
     cntxs_chunk_doknil_ref_type
     cntxs_chunk_ref
     cntxs_chunk_data_format &rest rs) row

    (make-fact
      :id fact_id
      :subj-id subj_id
      :subj-name subj_chunk_name
      :subj-ref-type subj_chunk_ref_type
      :subj-doknil-ref-type subj_chunk_doknil_ref_type
      :subj-ref subj_chunk_ref

      :subj-data-format subj_chunk_data_format
      :subj-doknil-data-format subj_chunk_doknil_data_format
      :role-id subj_role_id
      :role subj_role
      :doknil-role subj_doknil_role
      :is-part-of subj_is_part_of
      :cntxs (unless (equal cntx_max_pos :NULL)
        (iter
        (with r = (make-array (1+ cntx_max_pos)))
        (for i in-vector cntxs_pos)
        (for cntx =
           (make-cntx
             :pos i
             :role-id (aref cntxs_role_id i)
             :role (aref cntxs_role i)
             :doknil-role (aref cntxs_doknil_role i)
             :obj-id (aref cntxs_obj_id i)
             :obj-name (aref cntxs_chunk_name i)
             :obj-ref-type (aref cntxs_chunk_ref_type i)
             :obj-doknil-ref-type (aref cntxs_chunk_doknil_ref_type i)
             :obj-ref (aref cntxs_chunk_ref i)
             :obj-data-format (aref cntxs_chunk_data_format i)
             :obj-proposition (aref cntxs_proposition_name i)
                   ))
          (after-each
           (setf (aref r i) cntx))
          (finally (return r)))
    ))))



(defun get-about-chunk (chunk-id)
  ; TODO
  )

(defparameter +ui-query-limit+ 255
  "The maximum number of rows to retrieve in queries.")

; TODO find a way to use ? or similar in the queries
; TODO filter on obj must follow the part-of hierarchy
; TODO display the result in human-readable way for debugging porpouses
; TODO get all parent parts of a chunk
; TODO get all direct children of a chunk
; TODO create a reasonable KB specification and start asking queries
; TODO create a new polished repository
; TODO archive old info
; TODO put personal info in a private place
; TODO remove old repo
; TODO start from scratch with the repo of the project
; TODO generate a different SQL query according the type
; TODO use list* condition for mapping a specification with a context
; TODO add tests for all these features
(defun facts? (&rest rs)
  (flet ((is-not-? (s) (not (equal s '?))))

  (ematch rs
    ; (facts? subj/obj)
    ((guard (list subj)
            (is-not-? subj))

     (mapcar
        #'db-fact-with-cntx->fact
        (db-get-facts-about-chunk-id (db-get-chunk-id subj) +ui-query-limit+)))

    ; (facts? subj :isa role)
    ((guard (list subj :isa role)
            (and (is-not-? subj) (is-not-? role)))
     (mapcar
        #'db-fact-with-cntx->fact
        (db-get-facts-about-chunk-id-and-role-id
         (db-get-chunk-id subj)
         (db-get-role-id role)
         +ui-query-limit+)))

    ; (facts? subj :isa ? :of obj)
    ((guard (list subj :isa '? :of obj)
            (and (is-not-? subj) (is-not-? obj)))

     (mapcar
        #'db-fact-with-cntx->fact
        (db-get-facts-about-chunk-id-and-role-id
         (db-get-chunk-id subj)
         (db-get-chunk-id obj)
         +ui-query-limit+))))))

