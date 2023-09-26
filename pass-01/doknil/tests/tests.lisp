;; SPDX-License-Identifier: MIT
;; Copyright (C) 2022 Massimo Zaniboni <mzan@dokmelody.org>

(defpackage :dokmelody.tests
  (:import-from :alexandria
     :hash-table-keys)
  (:import-from :serapeum :let1)
  (:use :dokmelody)
  (:use :cl :trivia :parse-float :let-plus)
  (:use :cl-postgres :parachute)
  (:import-from :postmodern :connect-toplevel)
  (:import-from :postmodern :disconnect-toplevel))

(in-package :dokmelody.tests)

(define-test all
  :compile-at :execute

  (let ()

  (connect-toplevel
   "dokmelody" ; DB
   "dokmelody" ; ROLE
   "dokmelody" ; PASSWORD
   "127.0.0.1" ; DBMS HOST
   :application-name "dokmelody"
   :use-binary nil ; TODO there are types problems in preparated statements
  )

  (db-init)

(def-data-format /fmt1 /fmt2)

(def-data-format
      /a (/a1 /a2)
      /b (/b1
          /b2 (/c1 /c2)
          /b3
          /b4 (/d1 /d2))
      /e)

(def-role /part (/department /project (/task /issue /feature)))

(def-data-format
   /text (/html /markdown)
   /url (/image (/png /jpeg)))

(def-ref-type /name /url /file (/local /nfs))

(def-chunk doc1 :ref "readme.md" :ref-type /file/local :data-format /text/markdown)
(def-chunk doc2 :ref "readme.md" :ref-type /file/local)

(def-role
  /fiction (/novel /film /book)
  /person
  /group (/company /laboratory)
)

(def-chunk acme :ref "acme" :ref-type /name)
(def-chunk company1 :ref "company1" :ref-type /name)
(def-chunk dep1 :ref "dep1" :ref-type /name)
(def-chunk proj1 :ref "proj1" :ref-type /name)
(fact! company1 :isa /group/company)
(def-chunk task1 :ref "task1" :ref-type /name)
(def-chunk task2 :ref "task2" :ref-type /name)

(fact! acme :isa /group/company)

(def-role
  /responsible
  /customer
  /requester)

(def-role /part (/project (/feature /task /issue)
                 /department))

(fact! proj1 :isa /part/project :of dep1)
(fact! dep1 :isa /part/department :of company1)
(fact! dep1 :isa 'department :of company1)
(fact! task1 :isa /part/project/feature :of proj1)
(fact! acme :isa /customer)
(fact! acme :isa /requester :of task1)
(fact! acme :isa /requester :of task1 :in company1)

(cntx :in dep1
  (fact! proj1 :isa 'project))

(cntx :in dep1
  (cntx :in task1
    (fact! proj1 :isa 'project)
    (fact! task1 :isa /part/project/task :before task2)))

(facts? proj1)
(facts? task1)
(facts? proj1 :isa 'project))

  ; Tests
  ;
  (of-type integer 3)
  (true (numberp 2/3))
  (false (numberp :keyword))

  (disconnect-toplevel))

(test 'all)

(define-test version2
  :compile-at :execute

  (let ()

  (connect-toplevel
   "dokmelody" ; DB
   "dokmelody" ; ROLE
   "dokmelody" ; PASSWORD
   "127.0.0.1" ; DBMS HOST
   :application-name "dokmelody"
   :use-binary nil ; TODO there are types problems in preparated statements
  )

  (db-init)

  (def-cntx-role /date (/from /to))

      /quantity)

  (fact! grap-model ...)

  (fact! RDF :isa programming-language/db/kb)
  (fact! RDF :isa kb-specification/triple-store)
  (fact! WWW3C :isa sponsor :of RDF)
  (fact! Doknil :isa programming-language/db/kb)
  (fact! Doknil :isa kb-specification/role-based)
  (fact! Doknil :isa tool :of DokMelody)

  (def-role /part (/order (/item)))

  ; example of an order
  (fact! company1 :isa company)
  (fact! prod1 :isa product/office/chair)

  (fact! company1/order1 :isa order :of company1) ; equivalent to
  (fact! company1/order1 :isa /part/order)

  (fact! prod1 :isa item :of company1/order1 :for quantity/3)

  (fact! vendor1 :isa supplier :of prod1 :in company1/order1)
  (fact! employe1 :isa commercial-agent :of company1/order1)

  ; TODO connect different schema, writing rules of conversion and mapping these to derived views in PG
  ; TODO derive

  (cntx :in programming-language/CL
    (fact! fun#append :isa function)
    (fact! example1 :isa code/example :of fun#append)
    (fact! spec1 :isa spec :of fun#append)

    (fact! fun#nconc :and fun#append :are related)
    (fact! fun#concatenate :and fun#append :are related)
    ; equivalent to (fact! fun#concatenate :is related :to fun#append)
    ; and (fact! fun#concatenate :is related :to fun#append)

    (fact! package#overlord :isa package)
    (fact! package#overlord :isa tool/build-system)
    (fact! package#overlord :isa dependency-manager)

    (fact! package#proctor :isa user :of package#overlord)
  )

  (cntx :in programming
    (fact! concept#namespace :isa concept)
    (fact! concept#package :isa instance :of concept#namespace)
    (fact! concept#package )
  )

(def-data-format /fmt1 /fmt2)

(def-data-format
      /a (/a1 /a2)
      /b (/b1
          /b2 (/c1 /c2)
          /b3
          /b4 (/d1 /d2))
      /e)

(def-role /part (/department /project (/task /issue /feature)))

(def-data-format
   /text (/html /markdown)
   /url (/image (/png /jpeg)))

(def-ref-type /name /url /file (/local /nfs))

(def-chunk doc1 :ref "readme.md" :ref-type /file/local :data-format /text/markdown)
(def-chunk doc2 :ref "readme.md" :ref-type /file/local)

(def-role
  /fiction (/novel /film /book)
  /person
  /group (/company /laboratory)
)

(def-chunk acme :ref "acme" :ref-type /name)
(def-chunk company1 :ref "company1" :ref-type /name)
(def-chunk dep1 :ref "dep1" :ref-type /name)
(def-chunk proj1 :ref "proj1" :ref-type /name)
(fact! company1 :isa /group/company)
(def-chunk task1 :ref "task1" :ref-type /name)
(def-chunk task2 :ref "task2" :ref-type /name)

(fact! acme :isa /group/company)

(def-role
  /responsible
  /customer
  /requester)

(def-role /part (/project (/feature /task /issue)
                 /department))

(fact! proj1 :isa /part/project :of dep1)
(fact! dep1 :isa /part/department :of company1)
(fact! dep1 :isa 'department :of company1)
(fact! task1 :isa /part/project/feature :of proj1)
(fact! acme :isa /customer)
(fact! acme :isa /requester :of task1)
(fact! acme :isa /requester :of task1 :in company1)

(cntx :in dep1
  (fact! proj1 :isa 'project))

(cntx :in dep1
  (cntx :in task1
    (fact! proj1 :isa 'project)
    (fact! task1 :isa /part/project/task :before task2)))

(facts? proj1)
(facts? task1)
(facts? proj1 :isa 'project))

  ; Tests
  ;
  (of-type integer 3)
  (true (numberp 2/3))
  (false (numberp :keyword))

  (disconnect-toplevel))
